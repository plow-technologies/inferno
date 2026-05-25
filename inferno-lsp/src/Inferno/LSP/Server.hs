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
  ) where

import Colog.Core.Action (LogAction (LogAction))
import Colog.Core.Severity (WithSeverity)
import Control.Exception (SomeException)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (ReaderT), ask, runReaderT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
import qualified Inferno.LSP.DocState
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
import Inferno.LSP.Hover
  ( HoverEntry (HoverEntry),
    buildHoverIndex,
    linearize,
    queryHover,
    renderHoverContent,
  )
import qualified Inferno.LSP.Hover
import qualified Inferno.LSP.ParseInfer
import Inferno.LSP.ParseInfer
  ( InferSuccess (InferSuccess),
    parseAndInferDiagnostics,
  )
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
import UnliftIO.Async (Async, async, cancel)
import UnliftIO.Exception (catchAny)
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
    runLspWithConfig @c mods ctys $ f $ mkDefaultConfig tracer
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

    i <- LSP.Server.runServerWith ioLog lspLog cfg.clientIn cfg.clientOut serverDef
    traceWith cfg.tracer "shutting down..."
    pure i
  where
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
      let uri :: LSP.NormalizedUri
          uri = msg ^. LSP.params . LSP.textDocument . LSP.uri . to LSP.toNormalizedUri

          txt :: Text
          txt = msg ^. LSP.params . LSP.textDocument . LSP.text

      env <- lift ask
      tvar <- atomically $ openDoc uri env.docStore
      a <- liftIO . async $ do
        idents <- env.getIdents
        ts <- getCurrentTime
        uuid <- UUID.V4.nextRandom
        env.beforeParse (uuid, ts)
        parsed <- fromMaybe (Left [timeoutDiagnostic])
          <$> timeout 120_000_000 (pure (parseAndInferDiagnostics env.interpreter idents txt))
        atomically . updateDoc (either (const (emptyAnalysisResult 0)) (mkAnalysisResult 0) parsed) $ tvar
        let raw :: ParsedResult
            raw = toParsedResult . toInferResult env.allClasses =<< parsed
        diags <- either id (.warnings) . fromParsedResult <$> env.afterParse (uuid, ts) raw
        sendDiags env uri (Just 0) diags
      atomically $ setAnalysis a tvar

    handleDidChange :: LSP.Server.Handler (InfernoLspM c) LSP.TextDocumentDidChange
    handleDidChange = undefined

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
    toHover :: ((SourcePos, SourcePos), TypeMetadata TCScheme) -> (LSP.Range, LSP.MarkupContent)
    toHover ((s, e), meta) =
      renderHoverContent allClasses success.classes
        Inferno.LSP.Hover.HoverEntry{meta, start = s, end = e}

toParsedResult :: InferResult -> ParsedResult
toParsedResult r = Right (r.ast, r.scheme, r.warnings, r.hovers)

fromParsedResult :: ParsedResult -> Either [LSP.Diagnostic] InferResult
fromParsedResult = \case
  Left diags -> Left diags
  Right (ast, scheme, warnings, hovers) ->
    Right InferResult{ast, scheme, warnings, hovers}

mkAnalysisResult :: Int32 -> InferSuccess -> AnalysisResult
mkAnalysisResult ver success =
  AnalysisResult
    { version = ver
    , hoverIdx = buildHoverIndex success.typeMap
    , typeMap = success.typeMap
    , classes = success.classes
    }

emptyAnalysisResult :: Int32 -> AnalysisResult
emptyAnalysisResult ver =
  AnalysisResult
    { version = ver
    , hoverIdx = mempty
    , typeMap = mempty
    , classes = mempty
    }

sendDiags :: Env c -> LSP.NormalizedUri -> LSP.TextDocumentVersion -> [LSP.Diagnostic] -> IO ()
sendDiags = undefined

timeoutDiagnostic :: LSP.Diagnostic
timeoutDiagnostic = undefined
