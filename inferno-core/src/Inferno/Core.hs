{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Inferno.Core where

import Data.Bifunctor (bimap)
import qualified Data.List.NonEmpty as NEList
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Inferno.Eval (runEvalIO)
import Inferno.Eval.Error (EvalError)
import Inferno.Infer (inferExpr, inferTypeReps)
import Inferno.Infer.Pinned (pinExpr)
import Inferno.Module (Module (..))
import Inferno.Module.Builtin (builtinModule)
import Inferno.Module.Prelude (ModuleMap, baseOpsTable, builtinModulesOpsTable, builtinModulesPinMap, builtinModulesTerms)
import Inferno.Parse (parseExpr, prettyError)
import Inferno.Types.Syntax (Expr (App, TypeRep), ExtIdent, SourcePos, collectArrs)
import Inferno.Types.Type (ImplType (ImplType), TCScheme (ForallTC))
import Inferno.Types.Value (ImplEnvM, Value)
import Inferno.Types.VersionControl (VCObjectHash, pinnedToMaybe)
import Prettyprinter (Pretty)
import Text.Megaparsec (initialPos)

-- | Public API for the Inferno interpreter. @c@ is the custom value type.
data Interpreter c = Interpreter
  -- TODO do we need a instead of SourcePos?
  -- { eval :: forall a. Expr (Maybe VCObjectHash) a -> IO (Either EvalError (Value c (ImplEnvM IO c)))
  { eval ::
      Expr (Maybe VCObjectHash) SourcePos ->
      IO (Either EvalError (Value c (ImplEnvM IO c))),
    evalInEnv ::
      Map.Map ExtIdent (Value c (ImplEnvM IO c)) ->
      Map.Map ExtIdent (Value c (ImplEnvM IO c)) ->
      Expr (Maybe VCObjectHash) SourcePos ->
      IO (Either EvalError (Value c (ImplEnvM IO c))),
    parseAndEval ::
      Text ->
      IO (Either (Either String EvalError) (Value c (ImplEnvM IO c))),
    parseAndEvalInEnv ::
      Map.Map ExtIdent (Value c (ImplEnvM IO c)) ->
      Map.Map ExtIdent (Value c (ImplEnvM IO c)) ->
      Text ->
      IO (Either (Either String EvalError) (Value c (ImplEnvM IO c)))
      -- for LSP/onping etc: parse and type check?
  }

-- TODO next: plow-inferno, vp-calc, onping
-- Users: inferno (exe), inferno-vc-server, inferno-lsp-server, vp-calc, plow-inferno, onping, and tests!
-- vp-calc: builds own env, calls eval for script to VFun and runEvalIO to run
-- vp-calc: also splices in latestValue etc -- move that code here? Or use new instantiation?
-- onping: calls runLspServerWith and inferRuntimeTyReps

-- TODO inferno-tests Utils still instantiaties baseOpsTable etc. Make parse and parseAndInfer if someone else will use it?

mkInferno :: forall c. (Eq c, Pretty c) => ModuleMap IO c -> Interpreter c
mkInferno prelude =
  Interpreter
    { eval = evalInEnv Map.empty Map.empty,
      evalInEnv = evalInEnv,
      -- TODO still need above 2?
      parseAndEval = \src -> parseAndEvalInEnv Map.empty Map.empty src,
      parseAndEvalInEnv = parseAndEvalInEnv
    }
  where
    evalInEnv localEnv implEnv = runEvalIO @c (termEnv localEnv) implEnv

    parseAndEvalInEnv localEnv implEnv src = do
      -- parse
      case parseExpr (baseOpsTable prelude) (builtinModulesOpsTable prelude) src of
        Left err ->
          return $ Left $ Left $ "Failed parsing with: " <> (prettyError $ fst $ NEList.head err)
        Right (ast, _comments) -> do
          -- pin free variables to builtin prelude function hashes
          case pinExpr (builtinModulesPinMap prelude) ast of
            Left err -> return $ Left $ Left $ "Failed inference with: " <> show err
            Right pinnedAST -> do
              -- typecheck
              case inferExpr prelude pinnedAST of
                Left err -> return $ Left $ Left $ "Failed inference with: " <> show err
                Right (pinnedAST', sch@(ForallTC _ _ (ImplType _ typ)), _tyMap) -> do
                  let sig = collectArrs typ
                  let outTy = last sig
                  let inTys = init sig
                  -- infer runtime type-reps
                  case inferTypeReps allClasses sch inTys outTy of
                    Left err ->
                      return $ Left $ Left $ "Failed inference with: " <> show err
                    Right runtimeReps -> do
                      let finalAst =
                            foldl
                              App
                              -- TODO needs forall a in type Interpreter
                              -- (bimap pinnedToMaybe (const ()) pinnedAST')
                              -- [TypeRep () ty | ty <- runtimeReps]
                              (bimap pinnedToMaybe id pinnedAST')
                              [TypeRep (initialPos "dummy") ty | ty <- runtimeReps]
                      -- evaluate
                      evalInEnv localEnv implEnv finalAst >>= \case
                        Left err -> return $ Left $ Right err
                        Right res -> return $ Right res

    termEnv localEnv = ((localEnv, mempty) <>) <$> builtinModulesTerms prelude

    allClasses = Set.unions $ moduleTypeClasses builtinModule : [cls | Module {moduleTypeClasses = cls} <- Map.elems prelude]

-- TODO ask Sam if termEnv above is the same as:
-- mkEnv :: ImplEnvM IO () (TermEnv VCObjectHash () (ImplEnvM IO ()))
-- mkEnv = do
--   pinnedEnv <- snd <$> (builtinModulesTerms builtinModules)
--   pure (mempty, pinnedEnv)
