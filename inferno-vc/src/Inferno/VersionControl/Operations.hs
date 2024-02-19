{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Inferno.VersionControl.Operations
-- Description : Generic operations on the Inferno version control store
--
-- This module defines operations on the Inferno VC store.
module Inferno.VersionControl.Operations
  ( InfernoVCOperations (..),
    throwTyped
  )
where

import Control.Monad.Except (MonadError)
import Data.Generics.Sum (AsType(..))
import Data.Kind (Type)
import qualified Data.Set as Set
import Control.Monad.Error.Lens (throwing)
import Data.Time.Clock.POSIX (POSIXTime)
import Inferno.VersionControl.Operations.Error (VCStoreError)
import Inferno.Types.Syntax (getDependencies)
import Inferno.VersionControl.Types
  ( VCHashUpdate,
    VCMeta (..),
    VCObject (..),
    VCObjectHash (..),
  )

class
  ( Ord (Group m), -- This constraint is for fetchFunctionsForGroups's Set argument
    VCHashUpdate (Author m), -- These ones so we can hash a VCMeta
    VCHashUpdate (Group m),
    MonadError err m, -- These so implementors can throw VCStoreError when appropiate
    AsType VCStoreError err
  ) =>
  InfernoVCOperations err m
  where
  type Group m :: Type
  type Author m :: Type

  -- | Store an object and return its hash. hash is the object's primary key
  --   The following always holds (modulo errors):
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
  --   The following always holds (modulo errors):
  --   @
  --     x' <- storeVCObject x >>= fetchVCObject
  --     x == x'
  --   @
  fetchVCObject :: VCObjectHash -> m (VCMeta (Author m) (Group m) VCObject)

  -- | Fetch all dependencies of an object.
  fetchVCObjectClosureHashes :: VCObjectHash -> m [VCObjectHash]
  fetchVCObjectClosureHashes h0 = fmap (Set.toList . Set.delete h0) . go $ h0
    where
      go h = do
        o <- fetchVCObject h
        mconcat . (Set.singleton h :) <$> mapM go (Set.toList (getDependencies (obj o)))

  -- | Retrieves the full history of the chain which the given hash belongs to.
  -- History is given from newest (head) to oldest (root)
  fetchVCObjectHistory :: VCObjectHash -> m [VCMeta (Author m) (Group m) VCObjectHash]

  -- | Fetch all objects that are public or that belong to the given set of groups.
  -- Note this is a potentially long operation so no locks are held while traversing the
  -- store and checking every object making this operation weakly consistent.
  -- This means the returned list does not necessarily reflect the state of the store at any
  -- point in time.
  fetchFunctionsForGroups :: Set.Set (Group m) -> m [VCMeta (Author m) (Group m) VCObjectHash]

  -- | Delete all auto-saved objects older than a given time
  deleteAutosavedVCObjectsOlderThan :: POSIXTime -> m ()

throwTyped :: forall e err m x. (MonadError err m, AsType e err) => e -> m x
throwTyped = throwing _Typed
