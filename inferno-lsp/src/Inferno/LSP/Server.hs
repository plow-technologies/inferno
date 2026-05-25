{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (ReaderT), ask, runReaderT)
import qualified Control.Exception as Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Int (Int32)
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
import Inferno.LSP.Hover
  ( buildHoverIndex,
    linearize,
    queryHover,
    renderHoverContent,
  )
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
import Control.Lens
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
runLspWithConfig mods ctys cfg = undefined
