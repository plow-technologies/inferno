{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Inferno.Infer.Env
  ( Env (..),
    Namespace (..),
    TypeMetadata (..),
    closeOver,
    closeOverType,
    empty,
    lookup,
    lookupPinned,
    remove,
    extend,
    merge,
    mergeEnvs,
    singleton,
    keys,
    fromList,
    fromListModule,
    toList,
    normtype,
    normTC,
    fv,
    namespaceToIdent,
    generalize,
  )
where

import Data.Foldable (Foldable (foldl'))
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Inferno.Types.Syntax (ExtIdent)
import Inferno.Types.Type
  ( ImplType (..),
    InfernoType (..),
    Namespace (..),
    Substitutable (..),
    TCScheme (..),
    TV (..),
    TypeClass (..),
    TypeMetadata (..),
    namespaceToIdent,
  )
import Inferno.Types.VersionControl (VCObjectHash)
import Prelude hiding (lookup)

-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

data Env = TypeEnv
  { types :: Map.Map ExtIdent (TypeMetadata TCScheme),
    pinnedTypes :: Map.Map VCObjectHash (TypeMetadata TCScheme)
  }
  deriving (Eq, Show)

instance Substitutable Env where
  apply s env =
    env
      { types = Map.map (\meta -> meta {ty = apply s $ ty meta}) $ types env,
        pinnedTypes = Map.map (\meta -> meta {ty = apply s $ ty meta}) $ pinnedTypes env
      }
  ftv env =
    ftv $ map ty $ Map.elems $ types env

-- pinnedTypes should not have any free variables!!
empty :: Env
empty = TypeEnv Map.empty Map.empty

extend :: Env -> (ExtIdent, TypeMetadata TCScheme) -> Env
extend env (x, m) = env {types = Map.insert x m (types env)}

remove :: Env -> ExtIdent -> Env
remove env v = env {types = Map.delete v (types env)}

lookup :: ExtIdent -> Env -> Maybe (TypeMetadata TCScheme)
lookup key env = Map.lookup key (types env)

lookupPinned :: VCObjectHash -> Env -> Maybe (TypeMetadata TCScheme)
lookupPinned key env = Map.lookup key (pinnedTypes env)

merge :: Env -> Env -> Env
merge (TypeEnv a b) (TypeEnv a' b') =
  TypeEnv (Map.union a a') (Map.union b b')

mergeEnvs :: [Env] -> Env
mergeEnvs = foldl' merge empty

singleton :: ExtIdent -> TypeMetadata TCScheme -> Env
singleton x m =
  TypeEnv
    { types = Map.singleton x m,
      pinnedTypes = Map.empty
    }

keys :: Env -> [ExtIdent]
keys = Map.keys . types

fromList :: [(ExtIdent, TypeMetadata TCScheme)] -> Env
fromList xs =
  TypeEnv
    { types = Map.fromList xs,
      pinnedTypes = Map.empty
    }

fromListModule :: [(VCObjectHash, TypeMetadata TCScheme)] -> Env
fromListModule xs =
  TypeEnv
    { types = Map.empty,
      pinnedTypes = Map.fromList xs
    }

toList :: Env -> [(ExtIdent, TypeMetadata TCScheme)]
toList = Map.toList . types

instance Semigroup Env where
  (<>) = merge

instance Monoid Env where
  mempty = empty

normTC :: (InfernoType -> InfernoType) -> TypeClass -> TypeClass
normTC nt (TypeClass n tys) = TypeClass n (map nt tys)

fv :: InfernoType -> [TV]
fv (TVar a) = [a]
fv (TArr a b) = fv a ++ fv b
fv (TBase _) = []
fv (TArray t) = fv t
fv (TSeries t) = fv t
fv (TOptional t) = fv t
fv (TTuple ts) = foldr ((++) . fv) [] ts
fv (TRecord ts) = foldr ((++) . fv) [] ts
fv (TRep t) = fv t

normtype :: Map.Map TV TV -> InfernoType -> InfernoType
normtype ord (TArr a b) = TArr (normtype ord a) (normtype ord b)
normtype _ (TBase a) = TBase a
normtype ord (TArray a) = TArray $ normtype ord a
normtype ord (TSeries a) = TSeries $ normtype ord a
normtype ord (TOptional a) = TOptional $ normtype ord a
normtype ord (TTuple as) = TTuple $ fmap (normtype ord) as
normtype ord (TRecord as) = TRecord $ fmap (normtype ord) as
normtype ord (TRep a) = TRep $ normtype ord a
normtype ord (TVar a) =
  case Map.lookup a ord of
    Just x -> TVar x
    Nothing -> TVar a -- error $ "type variable " <> show a <> "not in signature"

normalize :: TCScheme -> TCScheme
normalize (ForallTC _ tcs (ImplType impl body)) =
  ForallTC
    (map snd ord)
    (Set.map (normTC $ normtype ordMap) tcs)
    $ ImplType (Map.map (normtype ordMap) impl) (normtype ordMap body)
  where
    -- collect free variables from the body of the function first,
    -- then from any implicit type variables and finally from the typeclasses
    ftvs = nub $ fv body ++ concatMap (fv . snd) (Map.toList impl) ++ concatMap (\(TypeClass _ tys) -> concatMap fv tys) (Set.toList tcs)
    ord = zip ftvs (map TV [0 ..])
    ordMap = Map.fromList ord

generalize :: Set.Set TypeClass -> ImplType -> TCScheme
generalize tcs t = ForallTC as tcs t
  where
    as = Set.toList $ ((ftv t) `Set.union` (Set.unions $ Set.elems $ Set.map ftv tcs))

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Set.Set TypeClass -> ImplType -> TCScheme
closeOver tcs t = normalize $ generalize tcs t

closeOverType :: InfernoType -> TCScheme
closeOverType = normalize . generalize Set.empty . ImplType Map.empty
