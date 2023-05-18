{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Inferno.ML.Module.Prelude where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (ExceptT, MonadError)
import Data.Bifunctor (bimap)
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Word (Word16, Word32, Word64)
import Inferno.Eval as Eval (TermEnv, runEvalIO)
import Inferno.Eval.Error (EvalError (..))
import Inferno.ML.Types.Value
import Inferno.Module.Cast (Either7, ToValue (toValue))
import qualified Inferno.Module.Prelude as Prelude
import Inferno.Parse (OpsTable)
import Inferno.Types.Module (PinnedModule)
import Inferno.Types.Syntax
  ( Expr (..),
    ExtIdent (..),
    ModuleName (..),
    Scoped (..),
  )
import Inferno.Types.Type (Namespace (..), TCScheme, TypeMetadata)
import Inferno.Types.Value (ImplEnvM, Value)
import Inferno.Types.VersionControl (Pinned (..), VCObjectHash)
import Inferno.Utils.QQ.Module (infernoModules)
import System.Posix.Types (EpochTime)
import qualified Torch as T

-- TODO this is a hack to redefine addition on tensors as well -- how to fix?
sumFun ::
  Either7 Double Int64 EpochTime Word16 Word32 Word64 T.Tensor ->
  Either7
    (Either Double Int64 -> Double)
    (Either Double Int64 -> Either Double Int64)
    (EpochTime -> EpochTime)
    (Word16 -> Word16)
    (Word32 -> Word32)
    (Word64 -> Word64)
    (T.Tensor -> T.Tensor)
sumFun =
  bimap (\x -> either ((+) x) ((+) x . fromIntegral)) $
    bimap (\i -> bimap ((+) $ fromIntegral i) ((+) i)) $
      bimap (+) $
        bimap (+) $
          bimap (+) $
            bimap (+) (+)

zerosFun :: [Int] -> T.Tensor
zerosFun = T.zeros'

mlModules :: (MonadError EvalError m) => Prelude.ModuleMap m MlValue
mlModules =
  [infernoModules|

module ML

  define addition on tensor tensor tensor;

  zeros : array of int -> tensor := ###zerosFun###;

|]

builtinModules :: Map.Map ModuleName (PinnedModule (ImplEnvM (ExceptT EvalError IO) MlValue (Eval.TermEnv VCObjectHash MlValue (ImplEnvM (ExceptT EvalError IO) MlValue))))
builtinModules =
  -- -- "Export" the TachDB module so that its functions can be used without a module prefix
  -- case Map.lookup "Base" modules of
  --   Nothing -> error "plow-inferno builtinModules: module Base not found"
  --   Just Module {moduleName, moduleOpsTable = opsTable, moduleTypeClasses = tyCls, moduleObjects = (nsMap, tyMap, mTrmEnv)} ->
  --     case Map.lookup "TachDB" modules of
  --       Nothing -> error "plow-inferno builtinModules: module TachDB not found"
  --       Just Module {moduleOpsTable = opsTable', moduleTypeClasses = tyCls', moduleObjects = (nsMap', tyMap', mTrmEnv')} ->
  --         Map.insert
  --           "Base"
  --           Module
  --             { moduleName,
  --               moduleOpsTable = IntMap.unionWith (<>) opsTable opsTable',
  --               moduleTypeClasses = tyCls <> tyCls',
  --               moduleObjects = (nsMap <> nsMap', tyMap <> tyMap', mTrmEnv >>= \x -> mTrmEnv' >>= \y -> pure $ x <> y)
  --             }
  --           (Map.delete "TachDB" modules)
  -- where
  --   modules =
  --     Map.unionWith
  --       (error "Duplicate module name in builtinModules")
  --       (Prelude.builtinModules @(ExceptT EvalError IO) @PlowValue)
  --       (plowModules @(ExceptT EvalError IO))
  Map.unionWith
    (error "Duplicate module name in builtinModules")
    (Prelude.builtinModules @(ExceptT EvalError IO) @MlValue)
    (mlModules @(ExceptT EvalError IO))

baseOpsTable :: OpsTable
baseOpsTable = Prelude.baseOpsTable @(ExceptT EvalError IO) @MlValue builtinModules

builtinModulesOpsTable :: Map.Map ModuleName OpsTable
builtinModulesOpsTable = Prelude.builtinModulesOpsTable @(ExceptT EvalError IO) @MlValue builtinModules

builtinModulesPinMap :: Map.Map (Scoped ModuleName) (Map.Map Namespace (Pinned VCObjectHash))
builtinModulesPinMap = Prelude.builtinModulesPinMap @(ExceptT EvalError IO) @MlValue builtinModules

builtinModulesTerms :: ImplEnvM (ExceptT EvalError IO) MlValue (Eval.TermEnv VCObjectHash MlValue (ImplEnvM (ExceptT EvalError IO) MlValue))
builtinModulesTerms = Prelude.builtinModulesTerms @(ExceptT EvalError IO) @MlValue builtinModules

preludeNameToTypeMap :: Map.Map (Maybe ModuleName, Namespace) (TypeMetadata TCScheme)
preludeNameToTypeMap = Prelude.preludeNameToTypeMap @(ExceptT EvalError IO) @MlValue builtinModules

runEvalIO ::
  (MonadCatch m) =>
  ImplEnvM (ExceptT EvalError m) MlValue (Eval.TermEnv VCObjectHash MlValue (ImplEnvM (ExceptT EvalError m) MlValue)) ->
  Map.Map ExtIdent (Value MlValue (ImplEnvM (ExceptT EvalError m) MlValue)) ->
  Expr (Maybe VCObjectHash) a ->
  m (Either EvalError (Value MlValue (ImplEnvM (ExceptT EvalError m) MlValue)))
runEvalIO = Eval.runEvalIO @_ @MlValue
