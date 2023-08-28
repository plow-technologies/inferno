{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Inferno.VersionControl.Operations
-- Description : Generic operations on the Inferno version control store
--
-- This module defines operations on the Inferno VC store. The store structure is as follows:
--
-- * `<storePath>` stores the JSON serialised `VCMeta VCObject`s, where the filename is the cryptographic hash (`VCOBjectHash`) of the object's contents
-- * `<storePath>/heads` is a set of current HEAD objects of the store, which can be seen as the roots of the VC tree. Each filename is the hash of an object,
--    and the file's contents are all the predecessors of this object, in chronological order, starting from the time it was created or cloned.
-- * `<storePath>/to_head` is a map from every `VCOBjectHash` to its current HEAD, where the file name is the source hash and the contents of the file are the HEAD hash
-- * `<storePath>/deps` is a map from every `VCOBjectHash` to its (transitive) dependencies, i.e. the file `<storePath>/deps/<hash>` describes the closure of `<hash>`
-- * Deleting `VCMeta VCObject` - Delete is implemented as soft delete. Object is moved to a directory called `removed`. Object's preds are also removed.
--   When an object is removed, its directory structure is preserved so you can undo it easily. i.e. `removed` directory has the same structure as `vc_store` directory.
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
  -- All scripts and their references are moved to "removed" directory
  deleteVCObjects :: VCObjectHash -> m ()

  fetchVCObject :: (Deserializable m a, Deserializable m g) => VCObjectHash -> m (VCMeta a g VCObject)

  -- | Fetch all dependencies of an object.
  fetchVCObjectClosureHashes :: VCObjectHash -> m [VCObjectHash]

  fetchVCObjectHistory :: (Deserializable m a, Deserializable m g) => VCObjectHash -> m [VCMeta a g VCObjectHash]

  -- | Fetch all objects that are public or that belong to the given set of groups.
  -- Note this is a potentially long operation so no locks are held while traversing the
  -- store and checking every object -- making this operation weakly consistent.
  -- This means the returned list does not necessarily reflect the state of the store at any
  -- point in time.
  fetchFunctionsForGroups :: (Ord g, Deserializable m a, Deserializable m g) => Set.Set g -> m [VCMeta a g VCObjectHash]

  getAllHeads :: m [VCObjectHash]
