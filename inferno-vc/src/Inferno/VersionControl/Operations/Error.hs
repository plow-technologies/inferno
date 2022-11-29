{-# LANGUAGE DeriveAnyClass #-}

module Inferno.VersionControl.Operations.Error where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Inferno.VersionControl.Types
  ( VCObjectHash (..),
  )

data VCStoreError
  = CouldNotDecodeObject VCObjectHash String
  | CouldNotFindObject VCObjectHash
  | CouldNotFindPath FilePath
  | CouldNotFindHead VCObjectHash
  | TryingToAppendToNonHead VCObjectHash
  | InvalidHash String
  | UnexpectedObjectType VCObjectHash Text
  | TryingToDeleteNonAutosave Text
  deriving (Show, Generic, ToJSON, FromJSON)

vcStoreErrorToString :: VCStoreError -> String
vcStoreErrorToString = \case
  CouldNotDecodeObject h s -> "Could not decode object '" <> show h <> "': " <> s
  CouldNotFindObject h ->
    "Could not find object '" <> show h <> "'"
  CouldNotFindPath fp -> "Could not find path: " <> fp
  CouldNotFindHead h -> "Could not find HEAD for object '" <> show h <> "'"
  TryingToAppendToNonHead h -> "Trying to append to non-HEAD object '" <> show h <> "'"
  InvalidHash s -> "Could not decode hash '" <> s <> "'"
  UnexpectedObjectType h s ->
    "Unexpected object type of '" <> show h <> "'. Was expecting " <> unpack s
  TryingToDeleteNonAutosave n -> "Trying to delete a non-autosaved script " <> unpack n
