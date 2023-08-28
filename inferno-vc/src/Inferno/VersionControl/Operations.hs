{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Inferno.VersionControl.Operations
-- Description : Generic operations on the Inferno version control store
--
-- This module defines operations on the Inferno VC store.
module Inferno.VersionControl.Operations
  ( InfernoVCOperations (..),
  )
where

import Control.Monad.Except (MonadError)
import Data.Generics.Sum (AsType (..))
import Data.Kind (Constraint, Type)
import qualified Data.Set as Set
import Inferno.VersionControl.Operations.Error (VCStoreError)
import Inferno.VersionControl.Types
  ( VCHashUpdate,
    VCMeta (..),
    VCObject (..),
    VCObjectHash (..),
  )

class (AsType VCStoreError err, MonadError err m) => InfernoVCOperations err m where
  type Serializable m :: Type -> Constraint
  type Deserializable m :: Type -> Constraint

  storeVCObject :: (VCHashUpdate a, VCHashUpdate g, Serializable m a, Serializable m g, Deserializable m a, Deserializable m g) => VCMeta a g VCObject -> m VCObjectHash

  -- | Delete a temporary object from the VC. This is used for autosaved scripts
  -- and to run tests against unsaved scripts
  deleteAutosavedVCObject :: VCObjectHash -> m ()

  -- | Soft delete script and its history (both predecessors and successors).
  deleteVCObjects :: VCObjectHash -> m ()

  fetchVCObject :: (Deserializable m a, Deserializable m g) => VCObjectHash -> m (VCMeta a g VCObject)

  -- | Fetch all dependencies of an object.
  fetchVCObjectClosureHashes :: VCObjectHash -> m [VCObjectHash]

  fetchVCObjectHistory :: (Deserializable m a, Deserializable m g) => VCObjectHash -> m [VCMeta a g VCObjectHash]

  -- | Fetch all objects that are public or that belong to the given set of groups.
  -- Note this is a potentially long operation so no locks are held while traversing the
  -- store and checking every object making this operation weakly consistent.
  -- This means the returned list does not necessarily reflect the state of the store at any
  -- point in time.
  fetchFunctionsForGroups :: (Ord g, Deserializable m a, Deserializable m g) => Set.Set g -> m [VCMeta a g VCObjectHash]

  getAllHeads :: m [VCObjectHash]
