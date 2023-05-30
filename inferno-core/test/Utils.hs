{-# LANGUAGE TypeApplications #-}

module Utils where

import qualified Data.Map as Map
import Inferno.Eval as Eval
import Inferno.Eval.Error (EvalError (..))
import qualified Inferno.Module.Prelude as Prelude
import Inferno.Parse (OpsTable)
import Inferno.Types.Module (PinnedModule)
import Inferno.Types.Syntax
  ( Expr (..),
    ExtIdent (..),
    ModuleName (..),
    Scoped (..),
  )
import Inferno.Types.Type (Namespace (..))
import Inferno.Types.Value (ImplEnvM, Value)
import Inferno.Types.VersionControl (Pinned (..), VCObjectHash)

type TestCustomValue = ()

builtinModules :: Map.Map ModuleName (PinnedModule (ImplEnvM IO TestCustomValue (Eval.TermEnv VCObjectHash TestCustomValue (ImplEnvM IO TestCustomValue))))
builtinModules = Prelude.builtinModules @IO @TestCustomValue

builtinModulesOpsTable :: Map.Map ModuleName OpsTable
builtinModulesOpsTable = Prelude.builtinModulesOpsTable @IO @TestCustomValue builtinModules

builtinModulesPinMap :: Map.Map (Scoped ModuleName) (Map.Map Namespace (Pinned VCObjectHash))
builtinModulesPinMap = Prelude.builtinModulesPinMap @IO @TestCustomValue builtinModules

baseOpsTable :: OpsTable
baseOpsTable = Prelude.baseOpsTable @IO @TestCustomValue builtinModules

builtinModulesTerms :: ImplEnvM IO TestCustomValue (Eval.TermEnv VCObjectHash TestCustomValue (ImplEnvM IO TestCustomValue))
builtinModulesTerms = Prelude.builtinModulesTerms @IO @TestCustomValue builtinModules

runEvalIO ::
  ImplEnvM IO TestCustomValue (TermEnv VCObjectHash TestCustomValue (ImplEnvM IO TestCustomValue)) ->
  Map.Map ExtIdent (Value TestCustomValue (ImplEnvM IO TestCustomValue)) ->
  Expr (Maybe VCObjectHash) a ->
  IO (Either EvalError (Value TestCustomValue (ImplEnvM IO TestCustomValue)))
runEvalIO = Eval.runEvalIO @TestCustomValue
