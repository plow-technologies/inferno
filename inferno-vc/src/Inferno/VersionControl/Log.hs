{-# LANGUAGE OverloadedStrings #-}

module Inferno.VersionControl.Log
  ( VCServerTrace (..),
    VCCacheTrace (..),
    vcServerTraceToText,
    vcCacheTraceToText,
  )
where

import Data.Text (Text, intercalate, pack)
import Inferno.VersionControl.Operations.Error (VCStoreError, vcStoreErrorToString)
import Inferno.VersionControl.Types (VCObjectHash)

data VCServerTrace
  = ThrownVCStoreError VCStoreError
  | ThrownVCOtherError Text
  | WriteJSON FilePath
  | WriteTxt FilePath
  | AlreadyExistsJSON FilePath
  | ReadJSON FilePath
  | ReadTxt FilePath
  | DeleteFile FilePath
  | VCFetchObjects [VCObjectHash]
  | VCFetchObjectClosureHashes VCObjectHash

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
  VCFetchObjects objs -> "FetchObjects " <> intercalate ", " (map (pack . show) objs)
  VCFetchObjectClosureHashes obj -> "FetchObjectClosureHashes " <> pack (show obj)

data VCCacheTrace
  = VCCacheHit VCObjectHash
  | VCCacheMiss VCObjectHash
  | VCCacheDepsHit VCObjectHash
  | VCCacheDepsMiss VCObjectHash

vcCacheTraceToText :: VCCacheTrace -> Text
vcCacheTraceToText = \case
  VCCacheHit h -> "✅ VC Cache hit " <> pack (show h)
  VCCacheMiss h -> "❌ VC Cache miss " <> pack (show h)
  VCCacheDepsHit h -> "✅ VC Cache deps hit " <> pack (show h)
  VCCacheDepsMiss h -> "❌ VC Cache deps miss " <> pack (show h)
