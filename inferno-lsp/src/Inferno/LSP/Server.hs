{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Inferno.LSP.Server
  ( LspConfig
      ( LspConfig,
        tracer,
        clientIn,
        clientOut,
        getIdents,
        validateIn,
        beforeParse,
        afterParse,
        debounceMs
      ),
    ParsedResult,
    runLsp,
  ) where

import Colog.Core.Action (LogAction (LogAction))
import Colog.Core.Severity (WithSeverity)
import Control.Concurrent (threadDelay)
import Control.Exception
  ( SomeException,
    displayException,
    evaluate,
    fromException,
  )
import Control.Lens
import Control.Monad (guard)
import Control.Monad.Extra (whenJustM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (ReaderT), ask, runReaderT)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Foldable (for_, traverse_) -- NOTE: Do NOT remove, needed for GHC version compat
import Data.Functor (($>))
import Data.Int (Int32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Utf16.Rope as Rope
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID.V4
import Inferno.Core (Interpreter, mkInferno)
import qualified Inferno.Core
import Inferno.LSP.Completion
  ( CompletionCtx (CompletionCtx),
    completionQueryAt,
    filterModuleNameCompletionItems,
    findInPrelude,
    identifierCompletionItems,
    mkCompletionItem,
    rwsCompletionItems,
  )
import Inferno.LSP.DocState
  ( AnalysisResult (AnalysisResult),
    DocState,
    DocStore,
    cancelAnalysis,
    closeDoc,
    lookupDoc,
    openDoc,
    setAnalysis,
    updateDoc,
  )
import qualified Inferno.LSP.DocState
import Inferno.LSP.Hover
  ( HoverEntry (HoverEntry),
    buildHoverIndex,
    linearize,
    queryHover,
    renderHoverContent,
  )
import qualified Inferno.LSP.Hover
import Inferno.LSP.ParseInfer
  ( InferSuccess (InferSuccess),
    parseAndInferDiagnostics,
  )
import qualified Inferno.LSP.ParseInfer
import Inferno.Module.Prelude (ModuleMap)
import Inferno.Types.Syntax
  ( CustomType,
    Expr,
    Ident (Ident),
    InfernoType,
    TCScheme,
    TypeClass,
    TypeMetadata,
  )
import Inferno.Types.VersionControl (Pinned, VCObjectHash)
import Language.LSP.Diagnostics (partitionBySource)
import qualified Language.LSP.Server as LSP.Server
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import qualified Language.LSP.VFS as LSP.VFS
import Plow.Logging (IOTracer, traceWith)
import Plow.Logging.Async (withAsyncHandleTracer)
import Prettyprinter (Pretty)
import qualified StmContainers.Map
import System.IO
  ( BufferMode (NoBuffering),
    hFlush,
    hSetBuffering,
    hSetEncoding,
    stderr,
    stdin,
    stdout,
    utf8,
  )
import System.Timeout (timeout)
import Text.Megaparsec.Pos (SourcePos)
import qualified Text.Megaparsec.Pos as Pos
import UnliftIO.Async (Async, AsyncCancelled, async, cancel)
import UnliftIO.Exception (catchAny, handleJust)
import UnliftIO.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
  )

-- | The monad stack for LSP handlers. @LspT@ provides access to LSP operations
-- (publish diagnostics, get virtual file, etc.) and @ReaderT (Env c) IO@
-- provides access to our own state via @lift ask@.
type InfernoLspM c = LSP.Server.LspT () (ReaderT (Env c) IO)

-- | Backwards-compatible result type for the @afterParse@ callback.
type ParsedResult =
  Either
    [LSP.Diagnostic]
    ( Expr (Pinned VCObjectHash) ()
    , TCScheme
    , [LSP.Diagnostic]
    , [(LSP.Range, LSP.MarkupContent)]
    )

-- | Internal success record; avoids working with the 4-tuple directly.
-- Converted to\/from 'ParsedResult' only at the @afterParse@ callback boundary.
data InferResult = InferResult
  { ast :: !(Expr (Pinned VCObjectHash) ())
  , scheme :: !TCScheme
  , warnings :: ![LSP.Diagnostic]
  , hovers :: ![(LSP.Range, LSP.MarkupContent)]
  }

-- | Configuration record for the LSP server. Built internally with sensible
-- defaults (stdio, stderr tracer, no-op callbacks, 150ms debounce); callers
-- supply a @LspConfig c -> LspConfig c@ to override what they need.
data LspConfig c = LspConfig
  { tracer :: !(IOTracer Text)
  , clientIn :: !(IO ByteString)
  , clientOut :: !(ByteString.Lazy.ByteString -> IO ())
  , getIdents :: !(IO [Maybe Ident])
  , validateIn :: !(InfernoType -> Either Text ())
  , beforeParse :: !((UUID, UTCTime) -> IO ())
  , afterParse :: !((UUID, UTCTime) -> ParsedResult -> IO ParsedResult)
  , debounceMs :: {-# UNPACK #-} !Int
  }

-- | Server environment, built once at startup from 'LspConfig' and threaded
-- through all handlers via 'ReaderT'.
data Env c = Env
  { docStore :: !DocStore
  , interpreter :: !(Interpreter IO c)
  , allClasses :: !(Set TypeClass)
  , tracer :: !(IOTracer Text)
  , getIdents :: !(IO [Maybe Ident])
  , beforeParse :: !((UUID, UTCTime) -> IO ())
  , afterParse :: !((UUID, UTCTime) -> ParsedResult -> IO ParsedResult)
  , validateIn :: !(InfernoType -> Either Text ())
  , debounceMs :: {-# UNPACK #-} !Int
  }

-- | Run the Inferno LSP server. Accepts a @ModuleMap@, @[CustomType]@, and a
-- config modifier. The modifier receives a default 'LspConfig' (stdio,
-- stderr tracer, no-op callbacks, 150ms debounce) and can override any field.
--
-- @
-- runLsp modules mempty id             -- uses defaults
-- runLsp modules mempty $ \\cfg -> cfg { debounceMs = 300 }
-- @
runLsp ::
  forall c.
  (Pretty c, Eq c) =>
  ModuleMap IO c ->
  [CustomType] ->
  (LspConfig c -> LspConfig c) ->
  IO Int
runLsp mods ctys f = do
  hSetBuffering stdin NoBuffering
  hSetEncoding stdin utf8
  hSetBuffering stdout NoBuffering
  hSetEncoding stdout utf8
  withAsyncHandleTracer stderr 100 $ \tracer ->
    runLspWithConfig @c mods ctys . f $ mkDefaultConfig tracer
  where
    mkDefaultConfig :: IOTracer Text -> LspConfig c
    mkDefaultConfig tracer =
      LspConfig
        { tracer
        , clientIn = ByteString.hGetSome stdin defaultChunkSize
        , clientOut = \bs -> ByteString.Lazy.hPut stdout bs *> hFlush stdout
        , getIdents = pure mempty
        , validateIn = const $ Right ()
        , beforeParse = const $ pure ()
        , afterParse = const pure
        , debounceMs = 150
        }

-- | Internal: run the server with a fully resolved config.
runLspWithConfig ::
  forall c. (Pretty c, Eq c) => ModuleMap IO c -> [CustomType] -> LspConfig c -> IO Int
runLspWithConfig mods ctys cfg =
  flip catchAny onErr $ do
    interpreter <- mkInferno mods ctys
    docStore <- StmContainers.Map.newIO

    let env :: Env c
        env =
          Env
            { docStore
            , interpreter
            , allClasses = interpreter.typeClasses
            , tracer = cfg.tracer
            , getIdents = cfg.getIdents
            , beforeParse = cfg.beforeParse
            , afterParse = cfg.afterParse
            , validateIn = cfg.validateIn
            , debounceMs = cfg.debounceMs
            }

        serverDef :: LSP.Server.ServerDefinition ()
        serverDef =
          LSP.Server.ServerDefinition
            { LSP.Server.defaultConfig = ()
            , LSP.Server.onConfigurationChange = \old _ -> Right old
            , LSP.Server.doInitialize = \lspEnv _ -> pure $ Right lspEnv
            , LSP.Server.staticHandlers = lspHandlers
            , LSP.Server.interpretHandler = \lspEnv ->
                LSP.Server.Iso (flip runReaderT env . LSP.Server.runLspT lspEnv) liftIO
            , LSP.Server.options = lspOptions
            }

        ioLog :: LogAction IO (WithSeverity LSP.Server.LspServerLog)
        ioLog = LogAction $ traceWith cfg.tracer . Text.pack . show

        lspLog :: LogAction (LSP.Server.LspM ()) (WithSeverity LSP.Server.LspServerLog)
        lspLog = LogAction $ liftIO . traceWith cfg.tracer . Text.pack . show

    LSP.Server.runServerWith ioLog lspLog cfg.clientIn cfg.clientOut serverDef
      >>= \i -> i <$ traceWith cfg.tracer "shutting down..."
  where
    -- Same handler for any synchronous (caught via `catchAny`) exception
    onErr :: SomeException -> IO Int
    onErr = (1 <$) . traceWith cfg.tracer . Text.pack . show

    lspHandlers :: LSP.Server.Handlers (InfernoLspM c)
    lspHandlers =
      mconcat
        [ LSP.Server.notificationHandler LSP.STextDocumentDidOpen handleDidOpen
        , LSP.Server.notificationHandler LSP.STextDocumentDidChange handleDidChange
        , LSP.Server.notificationHandler LSP.STextDocumentDidClose handleDidClose
        , LSP.Server.requestHandler LSP.STextDocumentHover handleHover
        , LSP.Server.requestHandler LSP.STextDocumentCompletion handleCompletion
        ]

    handleDidOpen :: LSP.Server.Handler (InfernoLspM c) LSP.TextDocumentDidOpen
    handleDidOpen msg = do
      env <- lift ask
      (tvar, old) <- atomically $ openDoc uri env.docStore
      liftIO $ traverse_ cancelAnalysis old
      a <- liftIO . async . swallowCancelled $ do
        idents <- env.getIdents
        ts <- getCurrentTime
        uuid <- UUID.V4.nextRandom
        env.beforeParse (uuid, ts)
        parsed <- runPipeline env idents txt

        let validated :: Either [LSP.Diagnostic] InferSuccess
            validated = validateInputs env.validateIn =<< parsed

            raw :: ParsedResult
            raw = toParsedResult . toInferResult env.allClasses =<< validated

        atomically
          . updateDoc
            ( either
                (const (emptyAnalysisResult ver))
                (mkAnalysisResult ver)
                validated
            )
          $ tvar

        -- Guard: only publish if no newer edit has superseded this cycle.
        -- After `updateDoc` sets `analysis = Nothing`, a racing `DidChange`
        -- cannot cancel us (it sees `Nothing`). Without this check, both the
        -- stale and fresh asyncs would call `sendDiags`, potentially
        -- overwriting current diagnostics with outdated ones.
        whenCurrentVersion tvar ver $
          publishAfterParse env uri ver (uuid, ts) raw
      atomically $ setAnalysis a tvar
      where
        uri :: LSP.NormalizedUri
        uri = msg ^. LSP.params . LSP.textDocument . LSP.uri . to LSP.toNormalizedUri

        ver :: Int32
        ver = msg ^. LSP.params . LSP.textDocument . LSP.version

        txt :: Text
        txt = msg ^. LSP.params . LSP.textDocument . LSP.text

    handleDidChange :: LSP.Server.Handler (InfernoLspM c) LSP.TextDocumentDidChange
    handleDidChange msg =
      whenJustM (LSP.Server.getVirtualFile uri) $ \vf ->
        lift ask >>= \env ->
          traverse_ (onChange env vf) =<< atomically (lookupDoc uri env.docStore)
      where
        uri :: LSP.NormalizedUri
        uri = msg ^. LSP.params . LSP.textDocument . LSP.uri . to LSP.toNormalizedUri

        onChange :: Env c -> LSP.VFS.VirtualFile -> TVar DocState -> InfernoLspM c ()
        onChange env vf tvar = do
          liftIO $ cancelAnalysis tvar
          a <- liftIO . async . swallowCancelled $ do
            threadDelay $ env.debounceMs * 1000
            idents <- env.getIdents
            ts <- getCurrentTime
            uuid <- UUID.V4.nextRandom
            env.beforeParse (uuid, ts)
            parsed <- runPipeline env idents txt

            let validated :: Either [LSP.Diagnostic] InferSuccess
                validated = validateInputs env.validateIn =<< parsed

            atomically
              . updateDoc
                ( either
                    (const (emptyAnalysisResult ver))
                    (mkAnalysisResult ver)
                    validated
                )
              $ tvar

            -- See version guard comment in `handleDidOpen`
            let raw :: ParsedResult
                raw = toParsedResult . toInferResult env.allClasses =<< validated

            whenCurrentVersion tvar ver $
              publishAfterParse env uri ver (uuid, ts) raw

          atomically $ setAnalysis a tvar
          where
            ver :: Int32
            ver = LSP.VFS.virtualFileVersion vf

            txt :: Text
            txt = LSP.VFS.virtualFileText vf

    handleDidClose :: LSP.Server.Handler (InfernoLspM c) LSP.TextDocumentDidClose
    handleDidClose = undefined

    handleHover :: LSP.Server.Handler (InfernoLspM c) LSP.TextDocumentHover
    handleHover = undefined

    handleCompletion :: LSP.Server.Handler (InfernoLspM c) LSP.TextDocumentCompletion
    handleCompletion = undefined

lspOptions :: LSP.Server.Options
lspOptions =
  LSP.Server.defaultOptions
    { LSP.Server.textDocumentSync = Just syncOptions
    , LSP.Server.executeCommandCommands = Nothing
    }

syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
  LSP.TextDocumentSyncOptions
    { LSP._openClose = Just True
    , LSP._change = Just LSP.TdSyncIncremental
    , LSP._willSave = Just False
    , LSP._willSaveWaitUntil = Just False
    , LSP._save = Just . LSP.InR $ LSP.SaveOptions (Just False)
    }

toInferResult :: Set TypeClass -> InferSuccess -> InferResult
toInferResult allClasses success =
  InferResult
    { ast = success.ast
    , scheme = success.scheme
    , warnings = success.warnings
    , hovers = fmap toHover . Map.toList $ success.typeMap
    }
  where
    toHover ::
      ((SourcePos, SourcePos), TypeMetadata TCScheme) -> (LSP.Range, LSP.MarkupContent)
    toHover ((s, e), meta) =
      renderHoverContent
        allClasses
        success.classes
        Inferno.LSP.Hover.HoverEntry{meta, start = s, end = e}

toParsedResult :: InferResult -> ParsedResult
toParsedResult r = Right (r.ast, r.scheme, r.warnings, r.hovers)

fromParsedResult :: ParsedResult -> Either [LSP.Diagnostic] InferResult
fromParsedResult = \case
  Left diags -> Left diags
  Right (ast, scheme, warnings, hovers) ->
    Right InferResult{ast, scheme, warnings, hovers}

mkAnalysisResult :: Int32 -> InferSuccess -> AnalysisResult
mkAnalysisResult version success =
  AnalysisResult
    { version
    , hoverIdx = buildHoverIndex success.typeMap
    , typeMap = success.typeMap
    , classes = success.classes
    }

emptyAnalysisResult :: Int32 -> AnalysisResult
emptyAnalysisResult version =
  AnalysisResult
    { version
    , hoverIdx = mempty
    , typeMap = mempty
    , classes = mempty
    }

-- | Wrap an IO action so that 'AsyncCancelled' is caught and silently
-- discarded. Used in analysis async bodies to ensure partial state updates
-- do not occur when a newer edit supersedes an in-flight pipeline.
swallowCancelled :: IO () -> IO ()
swallowCancelled = handleJust (fromException @AsyncCancelled) . const $ pure ()

-- | Execute an action only if the document version in the 'TVar' still matches
-- @ver@. This closes a race window: after 'updateDoc' sets @analysis = Nothing@,
-- a new @DidChange@ can arrive and spawn a fresh async (since 'cancelAnalysis' sees
-- Nothing and no-ops). Without this guard, both the old (stale) and new async
-- would publish diagnostics, potentially overwriting current results with
-- outdated ones. Reading the version via 'readTVarIO' is safe here because we
-- only need a point-in-time snapshot; the worst case is a harmless skip.
whenCurrentVersion :: TVar DocState -> Int32 -> IO () -> IO ()
whenCurrentVersion tvar ver f =
  readTVarIO tvar >>= \ds ->
    for_ @Maybe (guard (ds.version == ver)) . const $ f

-- | Run the parse\/infer pipeline with a 2-minute timeout. We force the
-- result to NF of the outer 'Either' AND the 'InferSuccess' constructor (whose
-- fields are all strict) inside the timeout window. Without the inner
-- 'evaluate', the 'Right' case would remain a thunk; the real inference work
-- would then execute later (during 'updateDoc') outside the timeout, defeating
-- its purpose.
runPipeline ::
  (Pretty c, Eq c) =>
  Env c ->
  [Maybe Ident] ->
  Text ->
  IO (Either [LSP.Diagnostic] InferSuccess)
runPipeline env idents txt =
  fmap (fromMaybe (Left [timeoutDiagnostic]))
    . timeout 120_000_000
    $ case parseAndInferDiagnostics env.interpreter idents txt of
      l@(Left _) -> evaluate l
      r@(Right s) -> evaluate s *> evaluate r

-- | Apply domain-level input type validation to a successful infer result.
-- If 'validateIn' rejects the input type, returns a single error diagnostic
-- at the start of the document.
validateInputs ::
  (InfernoType -> Either Text ()) ->
  InferSuccess ->
  Either [LSP.Diagnostic] InferSuccess
validateInputs validate success =
  first (pure . mkDiag) $ validate success.scheme.impl.body $> success
  where
    mkDiag :: Text -> LSP.Diagnostic
    mkDiag =
      Inferno.LSP.ParseInfer.mkDiagnostic
        LSP.DsError
        (Just "inferno.validate")
        (startPos, startPos)

    startPos :: SourcePos
    startPos = Pos.initialPos mempty

-- | Run the @afterParse@ callback and publish the resulting diagnostics,
-- catching any exception thrown by the callback. If @afterParse@ throws, we log
-- the error and skip publishing rather than letting the async die silently
-- (which would leave the user with no diagnostics until the next edit).
publishAfterParse ::
  Env c -> LSP.NormalizedUri -> Int32 -> (UUID, UTCTime) -> ParsedResult -> IO ()
publishAfterParse env uri ver key raw =
  catchAny publish $
    traceWith env.tracer
      . ("afterParse callback threw: " <>)
      . Text.pack
      . displayException
  where
    publish :: IO ()
    publish =
      sendDiags env uri (Just ver) . either id (.warnings) . fromParsedResult
        =<< env.afterParse key raw

sendDiags ::
  Env c ->
  LSP.NormalizedUri ->
  LSP.TextDocumentVersion ->
  [LSP.Diagnostic] ->
  IO ()
sendDiags = undefined

timeoutDiagnostic :: LSP.Diagnostic
timeoutDiagnostic = undefined
