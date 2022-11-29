{-# LANGUAGE TypeApplications #-}

module Utils where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (ExceptT)
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

builtinModules :: Map.Map ModuleName (PinnedModule (ImplEnvM (ExceptT EvalError IO) TestCustomValue (Eval.TermEnv VCObjectHash TestCustomValue (ImplEnvM (ExceptT EvalError IO) TestCustomValue))))
builtinModules = Prelude.builtinModules @(ExceptT EvalError IO) @TestCustomValue

builtinModulesOpsTable :: Map.Map ModuleName OpsTable
builtinModulesOpsTable = Prelude.builtinModulesOpsTable @(ExceptT EvalError IO) @TestCustomValue builtinModules

builtinModulesPinMap :: Map.Map (Scoped ModuleName) (Map.Map Namespace (Pinned VCObjectHash))
builtinModulesPinMap = Prelude.builtinModulesPinMap @(ExceptT EvalError IO) @TestCustomValue builtinModules

baseOpsTable :: OpsTable
baseOpsTable = Prelude.baseOpsTable @(ExceptT EvalError IO) @TestCustomValue builtinModules

builtinModulesTerms :: ImplEnvM (ExceptT EvalError IO) TestCustomValue (Eval.TermEnv VCObjectHash TestCustomValue (ImplEnvM (ExceptT EvalError IO) TestCustomValue))
builtinModulesTerms = Prelude.builtinModulesTerms @(ExceptT EvalError IO) @TestCustomValue builtinModules

runEvalIO ::
  (MonadCatch m) =>
  ImplEnvM (ExceptT EvalError m) TestCustomValue (Eval.TermEnv VCObjectHash TestCustomValue (ImplEnvM (ExceptT EvalError m) TestCustomValue)) ->
  Map.Map ExtIdent (Value TestCustomValue (ImplEnvM (ExceptT EvalError m) TestCustomValue)) ->
  Expr (Maybe VCObjectHash) a ->
  m (Either EvalError (Value TestCustomValue (ImplEnvM (ExceptT EvalError m) TestCustomValue)))
runEvalIO = Eval.runEvalIO @_ @TestCustomValue
