-- copy of all/alarm/alarm-log-server-chronicle/app/UnzipRequest.hs
{-# LANGUAGE OverloadedStrings #-}
-- See module description for the reasong for ignoring deprecations.
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Quarantines a function that uses a deprecated requestBody field.
-- We use Network.Wai's requestBody to create a response but it is deprecated.
-- But we are using it for the same purpose as they are, and they disable deprecations to work around their own deprecated field, so we do the same.
-- See https://github.com/yesodweb/wai/blob/e15f41ba20dbd94b511048692541ca89117f1f7c/wai/Network/Wai.hs#L34-L35
module Inferno.VersionControl.Server.UnzipRequest where

import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as BL
import Network.Wai

ungzipRequest :: Middleware
ungzipRequest app req = app req'
  where
    req'
      | Just "gzip" <- lookup "Content-encoding" (requestHeaders req) = go req
      | otherwise = req
    go r = r {requestBody = decompressNonEmpty <$> strictRequestBody r}
    decompressNonEmpty "" = "" -- Necessary 'cause the IO gets pulled until requestBody gives ""
    decompressNonEmpty x = BL.toStrict . decompress $ x
