{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bifunctor (bimap)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as Text
import Inferno.Eval (TermEnv, runEvalIO)
import Inferno.Infer (inferExpr, inferTypeReps)
import Inferno.Infer.Pinned (pinExpr)
import Inferno.ML.Module.Prelude (baseOpsTable, builtinModules, builtinModulesOpsTable, builtinModulesPinMap, builtinModulesTerms)
import Inferno.ML.Types.Value (MlValue)
import Inferno.Module.Builtin (builtinModule)
import Inferno.Module.Prelude (ModuleMap)
import Inferno.Parse (parseExpr)
import Inferno.Types.Module (Module (..))
import Inferno.Types.Syntax (Expr (..), collectArrs)
import Inferno.Types.Type (ImplType (ImplType), TCScheme (..))
import Inferno.Types.Value (ImplEnvM)
import Inferno.Types.VersionControl (VCObjectHash (..), pinnedToMaybe)
import Inferno.Utils.Prettyprinter (showPretty)
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  src <- Text.readFile file

  -- parse
  case parseExpr baseOpsTable builtinModulesOpsTable src of
    Left err -> print err
    Right (ast, _comments) -> do
      -- pin free variables to builtin prelude function hashes
      case pinExpr builtinModulesPinMap ast of
        Left err -> print err
        Right pinnedAST -> do
          -- typecheck
          case inferExpr prelude pinnedAST of
            Left err -> print err
            Right (pinnedAST', sch@(ForallTC _ _ (ImplType _ typ)), _tyMap) -> do
              let sig = collectArrs typ
              let outTy = last sig
              let inTys = init sig
              -- infer runtime type-reps
              case inferTypeReps allClasses sch inTys outTy of
                Left err -> print err
                Right runtimeReps -> do
                  let finalAst =
                        foldl
                          App
                          (bimap pinnedToMaybe (const ()) pinnedAST')
                          [TypeRep () ty | ty <- runtimeReps]
                  -- evaluate
                  runEvalIO mkEnv mempty finalAst >>= \case
                    Left err -> print err
                    Right res -> showPretty res
  where
    prelude :: ModuleMap IO MlValue
    prelude = builtinModules

    allClasses = Set.unions $ moduleTypeClasses builtinModule : [cls | Module {moduleTypeClasses = cls} <- Map.elems prelude]

    mkEnv :: ImplEnvM IO MlValue (TermEnv VCObjectHash MlValue (ImplEnvM IO MlValue))
    mkEnv = do
      pinnedEnv <- snd <$> builtinModulesTerms
      pure (mempty, pinnedEnv)
