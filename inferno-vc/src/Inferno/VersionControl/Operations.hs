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
import Data.Generics.Sum (AsType)
import Data.Kind (Type)
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (POSIXTime)
import Inferno.VersionControl.Operations.Error (VCStoreError)
import Inferno.VersionControl.Types
  ( VCHashUpdate,
    VCMeta (..),
    VCObject (..),
    VCObjectHash (..),
  )

class
  ( Ord (Group m),
    VCHashUpdate (Author m),
    VCHashUpdate (Group m),
    MonadError err m,
    AsType VCStoreError err
  ) =>
  InfernoVCOperations err m
  where
  type Group m :: Type
  type Author m :: Type

  -- | Store an object and return its hash. hash is the object's primary key
  --   The following always holds:
  --   @
  --     x' <- storeVCObject x >>= fetchVCObject
  --     x == x'
  --   @
  storeVCObject :: VCMeta (Author m) (Group m) VCObject -> m VCObjectHash

  -- | Delete a temporary object from the VC. This is used for autosaved scripts
  -- and to run tests against unsaved scripts
  deleteAutosavedVCObject :: VCObjectHash -> m ()

  -- | Soft delete script and its history (both predecessors and successors).
  deleteVCObjects :: VCObjectHash -> m ()

  -- | Fetch an object by its hash.
  --
  --   The following always holds:
  --   @
  --     x' <- storeVCObject x >>= fetchVCObject
  --     x == x'
  --   @
  fetchVCObject :: VCObjectHash -> m (VCMeta (Author m) (Group m) VCObject)

  -- | Fetch all dependencies of an object.
  fetchVCObjectClosureHashes :: VCObjectHash -> m [VCObjectHash]

  -- | Retrieves the full history of the chain which the given hash belongs to.
  -- History is given from newest (head) to oldest
  fetchVCObjectHistory :: VCObjectHash -> m [VCMeta (Author m) (Group m) VCObjectHash]

  -- | Fetch all objects that are public or that belong to the given set of groups.
  -- Note this is a potentially long operation so no locks are held while traversing the
  -- store and checking every object making this operation weakly consistent.
  -- This means the returned list does not necessarily reflect the state of the store at any
  -- point in time.
  fetchFunctionsForGroups :: Set.Set (Group m) -> m [VCMeta (Author m) (Group m) VCObjectHash]

  -- | Delete all auto-saved objects older than a given time
  deleteAutosavedVCObjectsOlderThan :: POSIXTime -> m ()
