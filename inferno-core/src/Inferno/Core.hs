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
import Inferno.Types.VersionControl (Pinned, VCObjectHash, pinnedToMaybe)
import Inferno.VersionControl.Types (VCObject (VCFunction))
import Prettyprinter (Pretty)
import Text.Megaparsec (initialPos)

data InfernoError
  = ParseError String
  | PinError String
  | InferenceError String
  deriving (Eq, Show)

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
    parseAndInferTypeReps ::
      Text ->
      Either InfernoError (Expr (Maybe VCObjectHash) SourcePos),
    parseAndInfer ::
      Text ->
      Either InfernoError (Expr (Pinned VCObjectHash) SourcePos, TCScheme),
    mkTermEnvWithOverrides ::
      Map.Map VCObjectHash VCObject ->
      Map.Map ExtIdent (Value c (ImplEnvM IO c)) ->
      [(Ident, Value c (ImplEnvM IO c))] ->
      ImplEnvM IO c (TermEnv VCObjectHash c (ImplEnvM IO c))
  }

mkInferno :: forall c. (Eq c, Pretty c) => ModuleMap IO c -> Interpreter c
mkInferno prelude =
  Interpreter
    { evalInEnv = \localEnv -> evalInImplEnvM (mkTermEnv localEnv),
      evalInImplEnvM = evalInImplEnvM,
      parseAndInferTypeReps = parseAndInferTypeReps,
      parseAndInfer = parseAndInfer,
      mkTermEnvWithOverrides = mkTermEnvWithOverrides -- TODO remove
    }
  where
    evalInImplEnvM termEnv implEnv = runEvalIO @c termEnv implEnv

    parseAndInfer src =
      -- parse
      case parseExpr (baseOpsTable prelude) (builtinModulesOpsTable prelude) src of
        Left err ->
          Left $ ParseError $ (prettyError $ fst $ NEList.head err)
        Right (ast, _comments) ->
          -- pin free variables to builtin prelude function hashes
          case pinExpr (builtinModulesPinMap prelude) ast of
            Left err -> Left $ PinError $ show err
            Right pinnedAST ->
              -- typecheck
              case inferExpr prelude pinnedAST of
                Left err -> Left $ InferenceError $ show err
                Right (pinnedAST', sch, _tyMap) ->
                  Right (pinnedAST', sch)

    parseAndInferTypeReps src =
      case parseAndInfer src of
        Left err -> Left err
        Right (pinnedAST', sch@(ForallTC _ _ (ImplType _ typ))) ->
          let sig = collectArrs typ
           in -- infer runtime type-reps
              case inferTypeReps allClasses sch (init sig) (last sig) of
                Left err ->
                  Left $ InferenceError $ show err
                Right runtimeReps ->
                  let finalAst =
                        foldl
                          App
                          (bimap pinnedToMaybe id pinnedAST')
                          [TypeRep (initialPos "dummy") ty | ty <- runtimeReps]
                   in Right finalAst

    allClasses = Set.unions $ moduleTypeClasses builtinModule : [cls | Module {moduleTypeClasses = cls} <- Map.elems prelude]

    mkTermEnv localEnv = ((localEnv, mempty) <>) <$> builtinModulesTerms prelude

    mkPinnedEnvFromClosure localEnv closure = do
      pinnedEnv <- snd <$> builtinModulesTerms prelude
      mdo
        pinnedEnv' <-
          foldM
            ( \env (hash, obj) -> case obj of
                VCFunction expr _ -> do
                  eval
                    (localEnv, pinnedEnv')
                    (bimap pinnedToMaybe id expr)
                    >>= \val ->
                      pure $ Map.insert hash val env
                _ -> pure env
            )
            pinnedEnv
            (Map.toList closure)
        pure (localEnv, pinnedEnv')

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
