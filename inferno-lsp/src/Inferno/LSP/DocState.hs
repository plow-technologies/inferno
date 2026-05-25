{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Inferno.LSP.DocState
  ( DocState
      ( DocState,
        version,
        hoverIdx,
        typeMap,
        classes,
        analysis,
        hoverCache
      ),
    AnalysisResult
      ( AnalysisResult,
        version,
        hoverIdx,
        typeMap,
        classes
      ),
    DocStore,
    lookupDoc,
    insertDoc,
    deleteDoc,
    cancelAnalysis,
    closeDoc,
    updateDoc,
    setAnalysis,
    openDoc,
    newDoc,
  ) where

import Data.Foldable (traverse_) -- NOTE: Do NOT remove, needed for GHC version compat
import Data.Int (Int32)
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Inferno.LSP.Hover (HoverIndex)
import Inferno.Types.Syntax (TCScheme, TypeClass, TypeMetadata)
import Language.LSP.Types (MarkupContent, NormalizedUri, Range)
import qualified StmContainers.Map
import Text.Megaparsec.Pos (SourcePos)
import UnliftIO.Async (Async, cancel)
import UnliftIO.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar',
    newTVar,
    readTVarIO,
    writeTVar,
  )

-- | Per-document state, stored in a 'TVar' inside the 'DocStore'. Replaced
-- atomically on each successful parse\/infer cycle.
data DocState = DocState
  { version :: {-# UNPACK #-} !Int32
  , hoverIdx :: !HoverIndex
  , typeMap :: !(Map (SourcePos, SourcePos) (TypeMetadata TCScheme))
  , classes :: !(Set TypeClass)
  , analysis :: !(Maybe (Async ()))
  , hoverCache :: !(IntMap (Range, MarkupContent))
  -- ^ Memoized hover results, keyed by linearized position (see
  -- 'Inferno.LSP.Hover.linearize'). Invalidated (set to @mempty@) on every
  -- analysis update since source positions shift.
  }

-- | Concurrent map from document URIs to their per-document state. Uses
-- @StmContainers.Map@ for lock-free concurrent access from LSP handlers.
type DocStore = StmContainers.Map.Map NormalizedUri (TVar DocState)

lookupDoc :: NormalizedUri -> DocStore -> STM (Maybe (TVar DocState))
lookupDoc = StmContainers.Map.lookup

insertDoc :: NormalizedUri -> TVar DocState -> DocStore -> STM ()
insertDoc uri tvar = StmContainers.Map.insert tvar uri

deleteDoc :: NormalizedUri -> DocStore -> STM ()
deleteDoc = StmContainers.Map.delete

-- | Cancel any in-flight analysis for a document. No-op if @analysis@ is
-- 'Nothing' or the async has already completed.
cancelAnalysis :: TVar DocState -> IO ()
cancelAnalysis tvar = traverse_ cancel . (.analysis) =<< readTVarIO tvar

-- | Handle @DidClose@: cancel in-flight analysis and remove the document
-- from the store.
closeDoc :: NormalizedUri -> DocStore -> IO ()
closeDoc uri store =
  traverse_ ((*> atomically (deleteDoc uri store)) . cancelAnalysis)
    =<< atomically (lookupDoc uri store)

-- | The output of a successful parse\/infer cycle, used to atomically replace
-- the relevant fields of 'DocState' via 'updateDoc'.
data AnalysisResult = AnalysisResult
  { version :: {-# UNPACK #-} !Int32
  , hoverIdx :: !HoverIndex
  , typeMap :: !(Map (SourcePos, SourcePos) (TypeMetadata TCScheme))
  , classes :: !(Set TypeClass)
  }

-- | Atomically replace document state with fresh analysis results. Clears the
-- hover cache since source positions may have shifted.
updateDoc :: AnalysisResult -> TVar DocState -> STM ()
updateDoc res tvar =
  writeTVar
    tvar
    DocState
      { version = res.version
      , hoverIdx = res.hoverIdx
      , typeMap = res.typeMap
      , classes = res.classes
      , analysis = Nothing
      , hoverCache = mempty
      }

-- | Record a newly spawned analysis async in the document state so it can be
-- cancelled if superseded by a newer edit.
setAnalysis :: Async () -> TVar DocState -> STM ()
setAnalysis a tvar = modifyTVar' tvar $ \ds -> ds{analysis = Just a}

-- | Handle @DidOpen@: create a fresh empty 'DocState' and insert it into the
-- store. Returns the 'TVar' for subsequent updates.
openDoc :: NormalizedUri -> DocStore -> STM (TVar DocState)
openDoc uri store =
  newDoc >>= \doc ->
    doc <$ insertDoc uri doc store

newDoc :: STM (TVar DocState)
newDoc =
  newTVar
    DocState
      { version = 0
      , hoverIdx = mempty
      , typeMap = mempty
      , classes = mempty
      , analysis = Nothing
      , hoverCache = mempty
      }
