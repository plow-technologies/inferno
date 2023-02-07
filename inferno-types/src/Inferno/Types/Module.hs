{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Inferno.Types.Module where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Inferno.Types.Syntax (Expr, ModuleName, OpsTable, GenericArbitrary (..))
import Inferno.Types.Type (Namespace, TCScheme, TypeClass, TypeMetadata)
import Inferno.Types.VersionControl (VCHashUpdate, VCObjectHash)
import Test.QuickCheck (Arbitrary (..))

data Module objs = Module
  { moduleName :: ModuleName,
    moduleOpsTable :: OpsTable,
    moduleTypeClasses :: Set.Set TypeClass,
    moduleObjects :: objs
  }
  deriving (Show, Eq, Generic)
  deriving Arbitrary via (GenericArbitrary (Module objs))
  deriving anyclass (VCHashUpdate, ToJSON, FromJSON)

newtype BuiltinModuleHash = BuiltinModuleHash ModuleName
  deriving stock (Generic)
  deriving anyclass (VCHashUpdate)

newtype BuiltinFunHash = BuiltinFunHash (Expr () (), TCScheme)
  deriving stock (Generic)
  deriving anyclass (VCHashUpdate)

newtype BuiltinEnumHash = BuiltinEnumHash TCScheme
  deriving stock (Generic)
  deriving anyclass (VCHashUpdate)

type PinnedModule m =
  Module (Map.Map Namespace VCObjectHash, Map.Map VCObjectHash (TypeMetadata TCScheme), m)

pinnedModuleNameToHash :: PinnedModule m -> Map.Map Namespace VCObjectHash
pinnedModuleNameToHash Module {moduleObjects = (hashes, _, _)} = hashes

pinnedModuleHashToTy :: PinnedModule m -> Map.Map VCObjectHash (TypeMetadata TCScheme)
pinnedModuleHashToTy Module {moduleObjects = (_, tys, _)} = tys

pinnedModuleTerms :: PinnedModule m -> m
pinnedModuleTerms Module {moduleObjects = (_, _, trms)} = trms
