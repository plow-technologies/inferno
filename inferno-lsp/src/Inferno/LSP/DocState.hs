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
      ( AnalysisResult
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
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Word (Word32)
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

data DocState = DocState
  { version :: {-# UNPACK #-} !Int32
  , hoverIdx :: !HoverIndex
  , typeMap :: !(Map (SourcePos, SourcePos) (TypeMetadata TCScheme))
  , classes :: !(Set TypeClass)
  , analysis :: !(Maybe (Async ()))
  , hoverCache :: !(Map (Word32, Word32) (Range, MarkupContent))
  }

type DocStore = StmContainers.Map.Map NormalizedUri (TVar DocState)

lookupDoc :: NormalizedUri -> DocStore -> STM (Maybe (TVar DocState))
lookupDoc = StmContainers.Map.lookup

insertDoc :: NormalizedUri -> TVar DocState -> DocStore -> STM ()
insertDoc uri tvar = StmContainers.Map.insert tvar uri

deleteDoc :: NormalizedUri -> DocStore -> STM ()
deleteDoc = StmContainers.Map.delete

cancelAnalysis :: TVar DocState -> IO ()
cancelAnalysis tvar = traverse_ cancel . (.analysis) =<< readTVarIO tvar

closeDoc :: NormalizedUri -> DocStore -> IO ()
closeDoc uri store =
  traverse_ ((*> atomically (deleteDoc uri store)) . cancelAnalysis)
    =<< atomically (lookupDoc uri store)

data AnalysisResult = AnalysisResult
  { version :: {-# UNPACK #-} !Int32
  , hoverIdx :: !HoverIndex
  , typeMap :: !(Map (SourcePos, SourcePos) (TypeMetadata TCScheme))
  , classes :: !(Set TypeClass)
  }

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

setAnalysis :: Async () -> TVar DocState -> STM ()
setAnalysis a tvar = modifyTVar' tvar $ \ds -> ds{analysis = Just a}

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
