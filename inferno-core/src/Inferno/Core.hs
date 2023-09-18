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
import Inferno.Module (Module (..))
import Inferno.Module.Builtin (builtinModule)
import Inferno.Module.Prelude (ModuleMap, baseOpsTable, builtinModulesOpsTable, builtinModulesPinMap, builtinModulesTerms)
import Inferno.Parse (parseExpr, prettyError)
import Inferno.Types.Syntax (Expr (App, TypeRep), ExtIdent, SourcePos, collectArrs)
import Inferno.Types.Type (ImplType (ImplType), TCScheme (ForallTC))
import Inferno.Types.Value (ImplEnvM, Value, runImplEnvM)
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
  { -- | Evaluates an Expr in a given pinned and implicit env. Use
    -- @defaultEnv@ for an empty env (only prelude) or compute one using
    -- @mkEnvFromClosure@.
    evalExpr ::
      forall a.
      TermEnv VCObjectHash c (ImplEnvM IO c) ->
      Map.Map ExtIdent (Value c (ImplEnvM IO c)) ->
      Expr (Maybe VCObjectHash) a ->
      IO (Either EvalError (Value c (ImplEnvM IO c))),
    parseAndInferTypeReps ::
      Text ->
      Either InfernoError (Expr (Maybe VCObjectHash) SourcePos),
    parseAndInfer ::
      Text ->
      Either InfernoError (Expr (Pinned VCObjectHash) SourcePos, TCScheme),
    -- | Evaluates all functions in given closure and creates a pinned env containing them
    mkEnvFromClosure ::
      Map.Map ExtIdent (Value c (ImplEnvM IO c)) ->
      Map.Map VCObjectHash VCObject ->
      ImplEnvM IO c (TermEnv VCObjectHash c (ImplEnvM IO c)),
    -- | The default pinned env containing only the prelude
    defaultEnv ::
      TermEnv VCObjectHash c (ImplEnvM IO c)
  }

mkInferno :: forall c. (Eq c, Pretty c) => ModuleMap IO c -> IO (Interpreter c)
mkInferno prelude = do
  -- We pre-compute envs that only depend on the prelude so that they can be
  -- shared among evaluations of different scripts
  (preludeIdentEnv, preludePinnedEnv) <- runImplEnvM Map.empty $ builtinModulesTerms prelude
  return $
    Interpreter
      { 
        evalExpr = runEvalIO,
        parseAndInferTypeReps = parseAndInferTypeReps,
        parseAndInfer = parseAndInfer,
        mkEnvFromClosure = mkEnvFromClosure preludePinnedEnv,
        defaultEnv = (preludeIdentEnv, preludePinnedEnv)
      }
  where
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

    -- TODO at some point: instead of evaluating closure and putting into pinned env,
    -- add closure into the expression being evaluated by using let bindings.
    mkEnvFromClosure preludePinnedEnv localEnv closure = do
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
            preludePinnedEnv
            (Map.toList closure)
        pure (localEnv, pinnedEnv')
