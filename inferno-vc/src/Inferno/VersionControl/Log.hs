module Inferno.VersionControl.Log where

import Inferno.VersionControl.Operations.Error (VCStoreError, vcStoreErrorToString)

data VCServerTrace
  = ThrownVCStoreError VCStoreError
  | WriteJSON FilePath
  | WriteTxt FilePath
  | AlreadyExistsJSON FilePath
  | ReadJSON FilePath
  | ReadTxt FilePath
  | DeleteFile FilePath

vcServerTraceToString :: VCServerTrace -> String
vcServerTraceToString = \case
  WriteJSON fp -> "Writing JSON at: " <> fp
  WriteTxt fp -> "Writing TXT at: " <> fp
  AlreadyExistsJSON fp -> "JSON already exists at: " <> fp
  ReadJSON fp -> "Reading JSON at: " <> fp
  ReadTxt fp -> "Reading TXT at: " <> fp
  ThrownVCStoreError e -> vcStoreErrorToString e
  DeleteFile fp -> "Deleting file: " <> fp
