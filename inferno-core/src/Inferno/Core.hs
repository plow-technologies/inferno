{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Inferno.Core where

import Control.Monad (foldM)
import Data.Bifunctor (bimap)
import qualified Data.List.NonEmpty as NEList
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Inferno.Eval (TermEnv, eval, runEvalIO)
import Inferno.Eval.Error (EvalError)
import Inferno.Infer (inferExpr, inferTypeReps)
import Inferno.Infer.Pinned (pinExpr)
import Inferno.Module (Module (..), pinnedModuleNameToHash)
import Inferno.Module.Builtin (builtinModule)
import Inferno.Module.Prelude (ModuleMap, baseOpsTable, builtinModulesOpsTable, builtinModulesPinMap, builtinModulesTerms)
import Inferno.Parse (parseExpr, prettyError)
import Inferno.Types.Syntax (Expr (App, TypeRep), ExtIdent, Ident, SourcePos, collectArrs)
import Inferno.Types.Type (ImplType (ImplType), Namespace (FunNamespace), TCScheme (ForallTC))
import Inferno.Types.Value (ImplEnvM, Value)
import Inferno.Types.VersionControl (VCObjectHash, pinnedToMaybe)
import Inferno.VersionControl.Types (VCObject (VCFunction))
import Prettyprinter (Pretty)
import Text.Megaparsec (initialPos)

-- | Public API for the Inferno interpreter. @c@ is the custom value type.
data Interpreter c = Interpreter
  { evalInEnv ::
      forall a.
      Map.Map ExtIdent (Value c (ImplEnvM IO c)) ->
      Map.Map ExtIdent (Value c (ImplEnvM IO c)) ->
      Expr (Maybe VCObjectHash) a ->
      IO (Either EvalError (Value c (ImplEnvM IO c))),
    evalInImplEnvM ::
      forall a.
      ImplEnvM IO c (TermEnv VCObjectHash c (ImplEnvM IO c)) ->
      Map.Map ExtIdent (Value c (ImplEnvM IO c)) ->
      Expr (Maybe VCObjectHash) a ->
      IO (Either EvalError (Value c (ImplEnvM IO c))),
    parseAndInfer ::
      Text ->
      Either String (Expr (Maybe VCObjectHash) SourcePos),
    mkTermEnvWithOverrides ::
      Map.Map VCObjectHash VCObject ->
      Map.Map ExtIdent (Value c (ImplEnvM IO c)) ->
      [(Ident, Value c (ImplEnvM IO c))] ->
      ImplEnvM IO c (TermEnv VCObjectHash c (ImplEnvM IO c))
      -- for LSP/onping etc: parse and type check?
  }

-- TODO next: plow-inferno, vp-calc, onping, parseAndInfer in LSP?
-- Users: inferno (exe), inferno-vc-server, inferno-lsp-server, vp-calc, plow-inferno, onping, and tests!
-- vp-calc: builds own env, calls eval for script to VFun and runEvalIO to run
-- vp-calc: also splices in latestValue etc -- move that code here? Or use new instantiation?
-- onping: calls runLspServerWith and inferRuntimeTyReps

-- TODO inferno-tests Utils still instantiaties baseOpsTable etc. Make parse and parseAndInfer if someone else will use it?

mkInferno :: forall c. (Eq c, Pretty c) => ModuleMap IO c -> Interpreter c
mkInferno prelude =
  Interpreter
    { -- eval = evalInEnv Map.empty Map.empty,
      evalInEnv = \localEnv -> evalInImplEnvM (mkTermEnv localEnv),
      evalInImplEnvM = evalInImplEnvM,
      -- TODO still need above 2?
      parseAndInfer = parseAndInfer,
      mkTermEnvWithOverrides = mkTermEnvWithOverrides
    }
  where
    evalInImplEnvM termEnv implEnv = runEvalIO @c termEnv implEnv

    parseAndInfer src =
      -- parse
      case parseExpr (baseOpsTable prelude) (builtinModulesOpsTable prelude) src of
        Left err ->
          Left $ "Failed parsing with: " <> (prettyError $ fst $ NEList.head err)
        Right (ast, _comments) ->
          -- pin free variables to builtin prelude function hashes
          case pinExpr (builtinModulesPinMap prelude) ast of
            Left err -> Left $ "Failed inference with: " <> show err
            Right pinnedAST ->
              -- typecheck
              case inferExpr prelude pinnedAST of
                Left err -> Left $ "Failed inference with: " <> show err
                Right (pinnedAST', sch@(ForallTC _ _ (ImplType _ typ)), _tyMap) ->
                  let sig = collectArrs typ
                   in -- infer runtime type-reps
                      case inferTypeReps allClasses sch (init sig) (last sig) of
                        Left err ->
                          Left $ "Failed inference with: " <> show err
                        Right runtimeReps ->
                          let finalAst =
                                foldl
                                  App
                                  (bimap pinnedToMaybe id pinnedAST')
                                  [TypeRep (initialPos "dummy") ty | ty <- runtimeReps]
                           in Right finalAst

    allClasses = Set.unions $ moduleTypeClasses builtinModule : [cls | Module {moduleTypeClasses = cls} <- Map.elems prelude]

    mkTermEnv localEnv = ((localEnv, mempty) <>) <$> builtinModulesTerms prelude

    mkTermEnvWithOverrides closure localEnv overrides = do
      let baseHashMap = pinnedModuleNameToHash (prelude Map.! "Base")
      pinnedEnv <- snd <$> builtinModulesTerms prelude
      let pinnedEnvOverridden =
            foldr
              ( \(name, val) env ->
                  Map.insert (baseHashMap Map.! (FunNamespace name)) val env
              )
              pinnedEnv
              overrides
      mdo
        pinnedEnv <-
          foldM
            ( \env (hash, obj) -> case obj of
                VCFunction expr _ -> do
                  eval
                    (localEnv, pinnedEnv)
                    (bimap pinnedToMaybe id expr)
                    >>= \val ->
                      pure $ Map.insert hash val env
                _ -> pure env
            )
            pinnedEnvOverridden
            (Map.toList closure)
        pure (localEnv, pinnedEnv)

-- TODO ask Sam if mkTermEnv above is the same as:
-- mkEnv :: ImplEnvM IO () (TermEnv VCObjectHash () (ImplEnvM IO ()))
-- mkEnv = do
--   pinnedEnv <- snd <$> (builtinModulesTerms builtinModules)
--   pure (mempty, pinnedEnv)