{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Inferno.VersionControl.Types
  ( VCObjectHash (..),
    VCObject (..),
    VCObjectVisibility (..),
    VCMeta (..),
    VCCommitMessage (..),
    VCIncompatReason (..),
    VCObjectPred (..),
    VCHashUpdate (..),
    Pinned (..),
    vcObjectHashToByteString,
    vcHash,
    showVCObjectType,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Foreign.C.Types (CTime)
import GHC.Generics (Generic)
import Inferno.Types.Module (Module (..))
import Inferno.Types.Syntax (Dependencies (..), Expr (..), GenericArbitrary (..), Ident (..))
import Inferno.Types.Type (Namespace, TCScheme (..)) -- TypeMetadata(..),
import Inferno.Types.VersionControl (Pinned (..), VCHashUpdate (..), VCObjectHash (..), pinnedUnderVCToMaybe, vcHash, vcObjectHashToByteString)
import Test.QuickCheck (Arbitrary (..), oneof)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)
import Test.QuickCheck.Instances.Text ()

data VCObject
  = VCModule (Module (Map Ident VCObjectHash))
  | VCFunction (Expr (Pinned VCObjectHash) ()) TCScheme -- (Map (SourcePos, SourcePos) (TypeMetadata TCScheme))
  | VCTestFunction (Expr (Pinned VCObjectHash) ())
  | VCEnum Ident (Set Ident)
  deriving (Show, Eq, Generic, ToJSON, FromJSON, VCHashUpdate)
  deriving Arbitrary via (GenericArbitrary VCObject)
  deriving anyclass ToADTArbitrary

-- TODO In order to show Arbitrary VCObject, we need the following instance.
-- Alternatively, we can just show the generic type is arbitrary as below.
-- See test/Parse/Spec's Arbitrary (Expr () ()) for an example, but note that it
-- only generates valid expressions. For golden tests we don't need to restrict ourselves
-- to valid expressions, so perhaps we can define a general Arbitrary instance in
-- Inferno.Types.Syntax?

instance Arbitrary (Expr (Pinned VCObjectHash) ()) where
  arbitrary = undefined -- TODO

-- instance (Arbitrary hash, Arbitrary pos) => Arbitrary (Expr hash pos) where
--   arbitrary = undefined -- TODO

showVCObjectType :: VCObject -> Text
showVCObjectType = \case
  VCModule _ -> "module"
  VCFunction _ _ -> "function"
  VCTestFunction _ -> "test function"
  VCEnum _ _ -> "enum"

instance Dependencies VCObject VCObjectHash where
  getDependencies = \case
    VCModule Module {moduleObjects = os} -> Set.fromList $ Map.elems os
    VCFunction expr _ -> Set.fromList $ catMaybes $ map pinnedUnderVCToMaybe $ Set.toList $ getDependencies expr
    VCTestFunction expr -> Set.fromList $ catMaybes $ map pinnedUnderVCToMaybe $ Set.toList $ getDependencies expr
    VCEnum _ _ -> mempty

data VCObjectVisibility = VCObjectPublic | VCObjectPrivate deriving (Show, Eq, Generic, ToJSON, FromJSON, VCHashUpdate)

instance Arbitrary VCObjectVisibility where
  arbitrary = oneof $ map pure [VCObjectPublic, VCObjectPrivate]

deriving instance ToADTArbitrary VCObjectVisibility

newtype VCCommitMessage = VCCommitMessage {unVCCommitMessage :: Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, VCHashUpdate)

data VCIncompatReason
  = TypeSignatureChange
  | EnumConstructorsChanged
  deriving (Show, Eq, Generic, ToJSON, FromJSON, VCHashUpdate)

instance Arbitrary VCIncompatReason where
  arbitrary = oneof $ map pure [TypeSignatureChange, EnumConstructorsChanged]

deriving instance ToADTArbitrary VCIncompatReason

data VCObjectPred
  = -- | Original script (root of the histories).
    Init
  | -- | Indicate whether a script could be automatically updated to a newer version, based on certain criteria,
    -- such as if the types and parameter order hasn't changed, If these checks pass the script should be marked as CompatibleWithPred.
    CompatibleWithPred VCObjectHash
  | -- | Like 'CompatibleWithPread' but if we automatically detect that the script would definitely be breaking,
    -- we should mark it as incompatible giving reason(s).
    IncompatibleWithPred VCObjectHash [(Namespace, VCIncompatReason)]
  | -- | MarkedBreakingWithPred should be used when the automatic detection declares the new script compatible,
    -- but we know we don't want to let it update automatically because the logic has changed.
    MarkedBreakingWithPred VCObjectHash
  | -- | Similar to 'Init' but this script is init'd by cloning the original script.
    CloneOf VCObjectHash
  | -- | CloneOfRemoved' is a "virtual" constructor to differentiate that the source of the script has been removed (but can
    -- still be found in removed directory). However, in the DB the field is still stored as 'CloneOf'. When we build the histories
    -- of a script, it will be differentiated between these 3 constructors for cloned script.
    CloneOfRemoved VCObjectHash
  | -- | 'CloneOfNotFound' is similar to 'CloneOfRemoved' but it is for case where the original script is not found
    -- i.e. the removed folder might get cleared so we lost the original script information.
    CloneOfNotFound VCObjectHash
  deriving (Show, Eq, Generic, ToJSON, FromJSON, VCHashUpdate)

instance Arbitrary VCObjectPred where
  arbitrary =
    oneof
      [ pure Init,
        CompatibleWithPred <$> arbitrary,
        IncompatibleWithPred <$> arbitrary <*> arbitrary,
        MarkedBreakingWithPred <$> arbitrary,
        CloneOf <$> arbitrary
      ]

deriving instance ToADTArbitrary VCObjectPred

-- the owner information and commit messages will be added in further revisions with other metadata as needed
data VCMeta author group o = VCMeta
  { timestamp :: CTime,
    author :: author,
    group :: group,
    name :: Text,
    description :: Text,
    pred :: VCObjectPred,
    -- commitMessage :: VCCommitMessage,
    visibility :: VCObjectVisibility,
    obj :: o
  }
  deriving (Show, Eq, Functor, Generic, ToJSON, FromJSON, VCHashUpdate)

instance (Arbitrary a, Arbitrary g, Arbitrary o) => Arbitrary (VCMeta a g o) where
  arbitrary =
    VCMeta
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

deriving instance (Arbitrary a, Arbitrary g, Arbitrary o) => ToADTArbitrary (VCMeta a g o)
