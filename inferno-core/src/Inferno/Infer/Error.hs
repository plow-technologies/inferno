{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Inferno.Infer.Error where

import Data.Functor.Foldable (cata, embed)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import qualified Data.Set as Set
import Inferno.Infer.Env (Namespace)
import Inferno.Infer.Exhaustiveness (Pattern)
import Inferno.Types.Syntax
  ( ExtIdent,
    Ident,
    ModuleName,
    Pat,
    Scoped,
  )
import Inferno.Types.Type
  ( InfernoType,
    Substitutable (..),
    TV,
    TypeClass,
  )
import Inferno.Types.VersionControl (Pinned, VCObjectHash)

type Location a = (a, a)

data TypeError a
  = UnificationFail (Set.Set TypeClass) InfernoType InfernoType (Location a)
  | AnnotationUnificationFail (Set.Set TypeClass) InfernoType InfernoType (Location a)
  | ExpectedFunction (Set.Set TypeClass) InfernoType InfernoType (Location a)
  | InfiniteType TV InfernoType (Location a)
  | UnboundNameInNamespace (Scoped ModuleName) (Either VCObjectHash Namespace) (Location a)
  | UnboundExtIdent (Scoped ModuleName) ExtIdent (Location a)
  | -- | UnificationMismatch (Set.Set TypeClass) InfernoType InfernoType (Location a)
    -- | Ambiguous [Constraint]
    ImplicitVarTypeOverlap (Set.Set TypeClass) ExtIdent InfernoType InfernoType (Location a)
  | VarMultipleOccurrence Ident (Location a) (Location a)
  | IfConditionMustBeBool (Set.Set TypeClass) InfernoType (Location a)
  | AssertConditionMustBeBool (Set.Set TypeClass) InfernoType (Location a)
  | IfBranchesMustBeEqType (Set.Set TypeClass) InfernoType InfernoType (Location a) (Location a)
  | CaseBranchesMustBeEqType (Set.Set TypeClass) InfernoType InfernoType (Location a) (Location a)
  | PatternUnificationFail InfernoType InfernoType (Pat (Pinned VCObjectHash) a) (Location a)
  | PatternsMustBeEqType (Set.Set TypeClass) InfernoType InfernoType (Pat (Pinned VCObjectHash) a) (Pat (Pinned VCObjectHash) a) (Location a) (Location a)
  | -- | TypeClassUnificationError InfernoType InfernoType TypeClass (Location a)
    TypeClassNotFoundError (Set.Set TypeClass) TypeClass (Location a)
  | TypeClassNoPartialMatch TypeClass (Location a)
  | CouldNotFindTypeclassWitness (Set.Set TypeClass) (Location a)
  | NonExhaustivePatternMatch Pattern (Location a)
  | UselessPattern (Maybe (Pat (Pinned VCObjectHash) a)) (Location a)
  | ModuleNameTaken ModuleName (Location a)
  | ModuleDoesNotExist ModuleName (Location a)
  | NameInModuleDoesNotExist ModuleName Ident (Location a)
  | AmbiguousName ModuleName Namespace (Location a)
  deriving (Show, Eq, Ord, Foldable)

makeBaseFunctor ''TypeError

instance Substitutable (TypeError a) where
  apply s = cata go
    where
      go :: TypeErrorF a (TypeError a) -> TypeError a
      go teF = embed $ fmap (apply s) teF

  ftv _ = Set.empty

getLocFromErr :: TypeError a -> [Location a]
getLocFromErr = go . foldr (:) []
  where
    go = \case
      [] -> []
      (x : y : xs) -> (x, y) : go xs
      _ -> undefined

getLocFromErrs :: [TypeError b] -> [Location b]
getLocFromErrs = concatMap getLocFromErr

getTypeClassFromErr :: TypeError a -> Set.Set TypeClass
getTypeClassFromErr = \case
  UnificationFail tyCls _ _ _ -> tyCls
  AnnotationUnificationFail tyCls _ _ _ -> tyCls
  ExpectedFunction tyCls _ _ _ -> tyCls
  ImplicitVarTypeOverlap tyCls _ _ _ _ -> tyCls
  IfConditionMustBeBool tyCls _ _ -> tyCls
  AssertConditionMustBeBool tyCls _ _ -> tyCls
  IfBranchesMustBeEqType tyCls _ _ _ _ -> tyCls
  CaseBranchesMustBeEqType tyCls _ _ _ _ -> tyCls
  PatternsMustBeEqType tyCls _ _ _ _ _ _ -> tyCls
  _ -> mempty

getTypeClassFromErrs :: [TypeError a] -> Set.Set TypeClass
getTypeClassFromErrs = foldr (Set.union . getTypeClassFromErr) mempty
