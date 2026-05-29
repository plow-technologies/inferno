{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
    runInfernoLspServer,
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
import Control.Monad (guard, join)
import Control.Monad.Extra (whenJustM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Foldable (for_, traverse_) -- NOTE: Do NOT remove, needed for GHC version compat
import Data.Function ((&))
import Data.Functor (($>))
import Data.Int (Int32)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Tuple (swap)
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
import qualified Inferno.LSP.Completion
import Inferno.LSP.DocState
  ( AnalysisResult (AnalysisResult),
    DocState (hoverCache),
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
    queryHover,
    renderHoverContent,
  )
import qualified Inferno.LSP.Hover
import Inferno.LSP.ParseInfer
  ( InferSuccess,
    parseAndInferDiagnostics,
  )
import qualified Inferno.LSP.ParseInfer
import Inferno.Module.Prelude (ModuleMap)
import Inferno.Types.Syntax
  ( CustomType,
    Expr,
    Ident,
    InfernoType,
    TCScheme,
    TypeClass,
    TypeMetadata,
  )
import qualified Inferno.Types.Syntax
import Inferno.Types.VersionControl (Pinned, VCObjectHash)
import Language.LSP.Diagnostics (partitionBySource)
import qualified Language.LSP.Server as LSP.Server
import qualified Language.LSP.Types as LSP hiding (line)
import qualified Language.LSP.Types.Lens as LSP
import qualified Language.LSP.VFS as LSP.VFS
import Lens.Micro ((^.))
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
import UnliftIO.Async (AsyncCancelled, async)
import UnliftIO.Exception (catchAny, handleJust)
import UnliftIO.STM
  ( STM,
    TVar,
    atomically,
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
  , clientOut :: !(LazyByteString -> IO ())
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

-- | Backwards-compatible entry point. Equivalent to @runLsp mods ctys id@.
runInfernoLspServer :: (Pretty c, Eq c) => ModuleMap IO c -> [CustomType] -> IO Int
runInfernoLspServer mods ctys = runLsp mods ctys id

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
      store <- lift $ asks (.docStore)
      (tvar, old) <- atomically $ openDoc uri store
      liftIO $ traverse_ cancelAnalysis old
      a <- async . swallowCancelled $ analyzeAndPublish tvar uri ver txt
      atomically $ setAnalysis a tvar
      where
        uri :: LSP.NormalizedUri
        uri = msg ^. LSP.params . LSP.textDocument . LSP.uri & LSP.toNormalizedUri

        ver :: Int32
        ver = msg ^. LSP.params . LSP.textDocument . LSP.version

        txt :: Text
        txt = msg ^. LSP.params . LSP.textDocument . LSP.text

    handleDidChange :: LSP.Server.Handler (InfernoLspM c) LSP.TextDocumentDidChange
    handleDidChange msg =
      whenJustM (LSP.Server.getVirtualFile uri) $ \vf ->
        traverse_ (onChange vf)
          =<< atomically . lookupDoc uri
          =<< lift (asks (.docStore))
      where
        uri :: LSP.NormalizedUri
        uri = msg ^. LSP.params . LSP.textDocument . LSP.uri & LSP.toNormalizedUri

        onChange :: LSP.VFS.VirtualFile -> TVar DocState -> InfernoLspM c ()
        onChange vf tvar = do
          liftIO $ cancelAnalysis tvar
          debounceMs <- lift $ asks (.debounceMs)
          a <- async . swallowCancelled $ do
            liftIO . threadDelay $ debounceMs * 1000
            analyzeAndPublish tvar uri ver txt
          atomically $ setAnalysis a tvar
          where
            ver :: Int32
            ver = LSP.VFS.virtualFileVersion vf

            txt :: Text
            txt = LSP.VFS.virtualFileText vf

    handleDidClose :: LSP.Server.Handler (InfernoLspM c) LSP.TextDocumentDidClose
    handleDidClose msg = liftIO . closeDoc uri =<< lift (asks (.docStore))
      where
        uri :: LSP.NormalizedUri
        uri = msg ^. LSP.params . LSP.textDocument . LSP.uri & LSP.toNormalizedUri

    handleHover :: LSP.Server.Handler (InfernoLspM c) LSP.TextDocumentHover
    handleHover req respond = do
      store <- lift $ asks (.docStore)
      classes <- lift $ asks (.allClasses)
      result <-
        liftIO . atomically $
          traverse (lookupHover classes) =<< lookupDoc uri store
      respond . Right $
        uncurry LSP.Hover . bimap LSP.HoverContents Just . swap <$> join result
      where
        uri :: LSP.NormalizedUri
        uri = req ^. LSP.params . LSP.textDocument . LSP.uri & LSP.toNormalizedUri

        line :: LSP.UInt
        line = req ^. LSP.params . LSP.position . LSP.line

        col :: LSP.UInt
        col = req ^. LSP.params . LSP.position . LSP.character

        -- Matches the encoding used by `Hover.linearize`: LSP positions are
        -- 0-indexed, `SourcePos` is 1-indexed with `- 1` applied in `linearize`,
        -- yielding the same `line * 10000 + col` value. This is also the key
        -- used in `DocState.hoverCache` (`IntMap`).
        pt :: Int
        pt = fromIntegral line * 1_0000 + fromIntegral col

        lookupHover ::
          Set TypeClass -> TVar DocState -> STM (Maybe (LSP.Range, LSP.MarkupContent))
        lookupHover classes tvar =
          readTVar tvar >>= \ds ->
            IntMap.lookup pt ds.hoverCache & \case
              hit@(Just _) -> pure hit
              Nothing -> do
                let computed :: Maybe (LSP.Range, LSP.MarkupContent)
                    computed =
                      renderHoverContent classes ds.classes
                        <$> queryHover (line, col) ds.hoverIdx
                for_ computed $ \r ->
                  writeTVar tvar ds{hoverCache = IntMap.insert pt r ds.hoverCache}
                pure computed

    handleCompletion :: LSP.Server.Handler (InfernoLspM c) LSP.TextDocumentCompletion
    handleCompletion req respond =
      LSP.Server.getVirtualFile uri >>= \case
        Nothing -> respond . Right . LSP.InL $ LSP.List mempty
        Just vf -> do
          interpreter <- lift $ asks (.interpreter)
          classes <- lift $ asks (.allClasses)
          idents <- liftIO =<< lift (asks (.getIdents))

          let txt :: Text
              txt = LSP.VFS.virtualFileText vf

              prefix :: Text
              (_, prefix) = completionQueryAt txt pos

              ctx :: CompletionCtx
              ctx = CompletionCtx{classes, prefix}

              preludeItems :: [LSP.CompletionItem]
              preludeItems =
                fmap (uncurry (mkCompletionItem ctx))
                  . Map.toList
                  $ findInPrelude interpreter.nameToTypeMap prefix

              identItems :: [LSP.CompletionItem]
              identItems =
                flip identifierCompletionItems prefix
                  . fmap (.unIdent)
                  . catMaybes
                  $ idents

              moduleItems :: [LSP.CompletionItem]
              moduleItems =
                filterModuleNameCompletionItems interpreter.nameToTypeMap prefix

              rwsItems :: [LSP.CompletionItem]
              rwsItems = rwsCompletionItems prefix

          respond . Right . LSP.InL . LSP.List $
            mconcat
              [ rwsItems
              , moduleItems
              , identItems
              , preludeItems
              ]
      where
        uri :: LSP.NormalizedUri
        uri = req ^. LSP.params . LSP.textDocument . LSP.uri & LSP.toNormalizedUri

        pos :: LSP.Position
        pos = req ^. LSP.params . LSP.position

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

-- | The core analysis cycle shared by @DidOpen@ and @DidChange@. Runs the
-- parse\/infer pipeline, updates the document state, and publishes diagnostics
-- via the @afterParse@ callback. Operates in 'InfernoLspM' so it has access to
-- both the LSP env (for 'publishDiagnostics') and our 'Env' (for config).
analyzeAndPublish ::
  (Pretty c, Eq c) =>
  TVar DocState ->
  LSP.NormalizedUri ->
  Int32 ->
  Text ->
  InfernoLspM c ()
analyzeAndPublish tvar uri ver txt = do
  env <- lift ask
  idents <- liftIO env.getIdents
  ts <- liftIO getCurrentTime
  uuid <- liftIO UUID.V4.nextRandom
  liftIO $ env.beforeParse (uuid, ts)
  parsed <- liftIO $ runPipeline env idents txt

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
    publishAfterParse uri ver (uuid, ts) raw

-- | Wrap an action so that 'AsyncCancelled' is caught and silently
-- discarded. Used in analysis async bodies to ensure partial state updates
-- do not occur when a newer edit supersedes an in-flight pipeline.
swallowCancelled :: InfernoLspM c () -> InfernoLspM c ()
swallowCancelled = handleJust (fromException @AsyncCancelled) . const $ pure ()

-- | Execute an action only if the document version in the 'TVar' still matches
-- @ver@. This closes a race window: after 'updateDoc' sets @analysis = Nothing@,
-- a new @DidChange@ can arrive and spawn a fresh async (since 'cancelAnalysis' sees
-- Nothing and no-ops). Without this guard, both the old (stale) and new async
-- would publish diagnostics, potentially overwriting current results with
-- outdated ones. Reading the version via 'readTVarIO' is safe here because we
-- only need a point-in-time snapshot; the worst case is a harmless skip.
whenCurrentVersion :: TVar DocState -> Int32 -> InfernoLspM c () -> InfernoLspM c ()
whenCurrentVersion tvar version f =
  readTVarIO tvar >>= \ds ->
    for_ @Maybe (guard (ds.version == version)) . const $ f

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
    -- NOTE We intentionally `evaluate` the inner types here, otherwise we'd
    -- only get the WHNF of the `Either` (i.e. if we just `evaluate`d
    -- `parseAndInferDiagnostics` directly)
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
  LSP.NormalizedUri ->
  Int32 ->
  (UUID, UTCTime) ->
  ParsedResult ->
  InfernoLspM c ()
publishAfterParse uri ver key raw = do
  tracer <- lift $ asks (.tracer)
  afterParse <- lift $ asks (.afterParse)
  catchAny (publish afterParse) $
    liftIO
      . traceWith tracer
      . ("afterParse callback threw: " <>)
      . Text.pack
      . displayException
  where
    publish :: ((UUID, UTCTime) -> ParsedResult -> IO ParsedResult) -> InfernoLspM c ()
    publish afterParse =
      sendDiags uri (Just ver) . either id (.warnings) . fromParsedResult
        =<< liftIO (afterParse key raw)

sendDiags ::
  LSP.NormalizedUri -> LSP.TextDocumentVersion -> [LSP.Diagnostic] -> InfernoLspM c ()
sendDiags uri ver =
  LSP.Server.publishDiagnostics 100 uri ver . partitionBySource

timeoutDiagnostic :: LSP.Diagnostic
timeoutDiagnostic =
  LSP.Diagnostic
    { _range = LSP.mkRange 0 0 0 0
    , _severity = Just LSP.DsError
    , _code = Nothing
    , _source = Just "inferno.lsp"
    , _message = "Inferno timed out in 120s"
    , _tags = Nothing
    , _relatedInformation = Nothing
    }
