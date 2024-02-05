{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Inferno.Core where

import Control.Monad (foldM)
import Control.Monad.Catch (MonadCatch)
import Data.Bifunctor (bimap)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Inferno.Eval (TermEnv, runEvalM)
import Inferno.Eval.Error (EvalError)
import Inferno.Infer (TypeError, inferExpr, inferTypeReps)
import Inferno.Infer.Error (Location)
import Inferno.Infer.Pinned (pinExpr)
import Inferno.Module (Module (..), Prelude (..), baseOpsTable, moduleOpsTables, preludeNameToTypeMap, preludePinMap, preludeTermEnv)
import Inferno.Module.Builtin (builtinModule)
import Inferno.Parse (InfernoParsingError, parseExpr)
import Inferno.Types.Syntax (Comment, CustomType, Expr (App, TypeRep), ExtIdent, ModuleName, Namespace, SourcePos, TypeClass, TypeMetadata, collectArrs)
import Inferno.Types.Type (ImplType (ImplType), TCScheme (ForallTC))
import Inferno.Types.Value (ImplEnvM, Value)
import Inferno.Types.VersionControl (Pinned, VCObjectHash, pinnedToMaybe)
import Inferno.VersionControl.Types (VCObject (VCFunction))
import Prettyprinter (Pretty)
import Text.Megaparsec (ParseError)

data InfernoError
  = ParseError (NonEmpty (ParseError Text InfernoParsingError, SourcePos))
  | PinError [TypeError SourcePos]
  | InferenceError [TypeError SourcePos]
  deriving (Eq, Show)

-- | Public API for the Inferno interpreter.
-- @m@ is the monad to run the interpreter in.
-- @c@ is the custom value type.
data Interpreter m c = Interpreter
  { -- | Evaluates an Expr in a given pinned and implicit env. Use
    -- @defaultEnv@ for an empty env (only prelude) or compute one using
    -- @mkEnvFromClosure@.
    evalExpr ::
      forall a.
      TermEnv VCObjectHash c (ImplEnvM m c) a ->
      Map.Map ExtIdent (Value c (ImplEnvM m c)) ->
      Expr (Maybe VCObjectHash) a ->
      m (Either EvalError (Value c (ImplEnvM m c))),
    parseAndInferTypeReps ::
      Text ->
      Either InfernoError (Expr (Maybe VCObjectHash) ()),
    parseAndInfer ::
      Text ->
      Either InfernoError (Expr (Pinned VCObjectHash) SourcePos, TCScheme, Map.Map (Location SourcePos) (TypeMetadata TCScheme), [Comment SourcePos]),
    -- | Evaluates all functions in given closure and creates a pinned env containing them
    mkEnvFromClosure ::
      Map.Map ExtIdent (Value c (ImplEnvM m c)) ->
      Map.Map VCObjectHash VCObject ->
      ImplEnvM m c (TermEnv VCObjectHash c (ImplEnvM m c) ()),
    -- | The default pinned env containing only the prelude
    defaultEnv ::
      TermEnv VCObjectHash c (ImplEnvM m c) (),
    -- | The type of each name in this interpreter's prelude
    nameToTypeMap ::
      Map.Map (Maybe ModuleName, Namespace) (TypeMetadata TCScheme),
    -- | The set of all type classes in this interpreter's prelude
    typeClasses ::
      Set.Set TypeClass
  }

mkInferno :: forall m c. (MonadCatch m, Pretty c) =>Prelude m c -> [CustomType] -> m (Interpreter m c)
mkInferno prelude@(Prelude {moduleMap}) customTypes = do
  -- We pre-compute envs that only depend on the prelude so that they can be
  -- shared among evaluations of different scripts
  let (preludeIdentEnv, preludePinnedEnv) = preludeTermEnv prelude
  return $
    Interpreter
      { evalExpr = runEvalM,
        parseAndInferTypeReps = parseAndInferTypeReps,
        parseAndInfer = parseAndInfer,
        mkEnvFromClosure = mkEnvFromClosure preludePinnedEnv,
        defaultEnv = (preludeIdentEnv, preludePinnedEnv),
        nameToTypeMap = preludeNameToTypeMap prelude,
        typeClasses = typeClasses
      }
  where
    parseAndInfer src =
      -- parse
      case parseExpr (baseOpsTable prelude) (moduleOpsTables prelude) customTypes src of
        Left err ->
          Left $ ParseError err
        Right (ast, comments) ->
          -- pin free variables to builtin prelude function hashes
          case pinExpr (preludePinMap prelude) ast of
            Left err -> Left $ PinError err
            Right pinnedAST ->
              -- typecheck
              case inferExpr moduleMap pinnedAST of
                Left err -> Left $ InferenceError err
                Right (pinnedAST', sch, tyMap) ->
                  Right (pinnedAST', sch, tyMap, comments)

    parseAndInferTypeReps src =
      case parseAndInfer src of
        Left err -> Left err
        Right (pinnedAST', sch@(ForallTC _ _ (ImplType _ typ)), _tyMap, _comments) ->
          let sig = collectArrs typ
           in -- infer runtime type-reps
              case inferTypeReps typeClasses sch (init sig) (last sig) of
                Left err ->
                  Left $ InferenceError err
                Right runtimeReps ->
                  let finalAst =
                        foldl
                          App
                          (bimap pinnedToMaybe (const ()) pinnedAST')
                          [TypeRep () ty | ty <- runtimeReps]
                   in Right finalAst

    typeClasses = Set.unions $ moduleTypeClasses builtinModule : [cls | Module {moduleTypeClasses = cls} <- Map.elems moduleMap]

    -- TODO at some point: instead of evaluating closure and putting into pinned env,
    -- add closure into the expression being evaluated by using let bindings.
    mkEnvFromClosure preludePinnedEnv localEnv closure = do
      mdo
        pinnedEnv' <-
          foldM
            ( \env (hash, obj) -> case obj of
                VCFunction expr _ -> do
                  let expr' = bimap pinnedToMaybe id expr
                  pure $ Map.insert hash (Left expr') env
                _ -> pure env
            )
            preludePinnedEnv
            (Map.toList closure)
        pure (localEnv, pinnedEnv')
