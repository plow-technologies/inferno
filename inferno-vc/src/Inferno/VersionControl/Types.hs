{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Foreign.C.Types (CTime)
import GHC.Generics (Generic)
import Inferno.Types.Module (Module (..))
import Inferno.Types.Syntax (Dependencies (..), Expr (..), Ident (..))
import Inferno.Types.Type (Namespace, TCScheme (..))
import Inferno.Types.VersionControl (Pinned (..), VCHashUpdate (..), VCObjectHash (..), pinnedUnderVCToMaybe, vcHash, vcObjectHashToByteString)

data VCObject
  = VCModule (Module (Map Ident VCObjectHash))
  | VCFunction (Expr (Pinned VCObjectHash) ()) TCScheme -- (Map (SourcePos, SourcePos) (TypeMetadata TCScheme))
  | VCTestFunction (Expr (Pinned VCObjectHash) ())
  | VCEnum Ident (Set Ident)
  deriving (Show, Eq, Generic, ToJSON, FromJSON, VCHashUpdate)

showVCObjectType :: VCObject -> Text
showVCObjectType = \case
  VCModule _ -> "module"
  VCFunction _ _ -> "function"
  VCTestFunction _ -> "test function"
  VCEnum _ _ -> "enum"

instance Dependencies VCObject VCObjectHash where
  getDependencies = \case
    VCModule Module{moduleObjects = os} -> Set.fromList $ Map.elems os
    VCFunction expr _ -> Set.fromList $ mapMaybe pinnedUnderVCToMaybe (Set.toList $ getDependencies expr)
    VCTestFunction expr -> Set.fromList $ mapMaybe pinnedUnderVCToMaybe (Set.toList $ getDependencies expr)
    VCEnum _ _ -> mempty

data VCObjectVisibility = VCObjectPublic | VCObjectPrivate deriving (Show, Eq, Generic, ToJSON, FromJSON, VCHashUpdate)

newtype VCCommitMessage = VCCommitMessage {unVCCommitMessage :: Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, VCHashUpdate)

data VCIncompatReason
  = TypeSignatureChange
  | EnumConstructorsChanged
  deriving (Show, Eq, Generic, ToJSON, FromJSON, VCHashUpdate)

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

-- the owner information and commit messages will be added in further revisions with other metadata as needed
data VCMeta author group o = VCMeta
  { timestamp :: CTime
  , author :: author
  , group :: group
  , name :: Text
  , description :: Text
  , pred :: VCObjectPred
  , -- commitMessage :: VCCommitMessage,
    visibility :: VCObjectVisibility
  , obj :: o
  }
  deriving (Show, Eq, Functor, Generic, ToJSON, FromJSON, VCHashUpdate)
