{-# LANGUAGE OverloadedStrings #-}

module Inferno.VersionControl.Log where

import Data.Text (Text, pack)
import Inferno.VersionControl.Operations.Error (VCStoreError, vcStoreErrorToString)

data VCServerTrace
  = ThrownVCStoreError VCStoreError
  | ThrownVCOtherError Text
  | WriteJSON FilePath
  | WriteTxt FilePath
  | AlreadyExistsJSON FilePath
  | ReadJSON FilePath
  | ReadTxt FilePath
  | DeleteFile FilePath

vcServerTraceToText :: VCServerTrace -> Text
vcServerTraceToText = \case
  WriteJSON fp -> "Writing JSON at: " <> pack fp
  WriteTxt fp -> "Writing TXT at: " <> pack fp
  AlreadyExistsJSON fp -> "JSON already exists at: " <> pack fp
  ReadJSON fp -> "Reading JSON at: " <> pack fp
  ReadTxt fp -> "Reading TXT at: " <> pack fp
  ThrownVCStoreError e -> pack (vcStoreErrorToString e)
  ThrownVCOtherError e -> "Other server error: " <> e
  DeleteFile fp -> "Deleting file: " <> pack fp
