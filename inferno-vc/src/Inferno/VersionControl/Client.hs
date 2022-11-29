{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Inferno.VersionControl.Client where

import Codec.Compression.GZip (compress)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy (Proxy (..))
import Inferno.VersionControl.Server (VCServerError, VersionControlAPI)
import Network.HTTP.Client (Request (..), RequestBody (..))
import Network.HTTP.Client.Internal (Manager (..))
import Network.HTTP.Types.Header (hContentEncoding)
import Servant.Client (BaseUrl, Client, ClientEnv, ClientM, client, mkClientEnv)
import Servant.Typed.Error (TypedClientM)

mkVCClientEnv :: Manager -> BaseUrl -> ClientEnv
mkVCClientEnv man@Manager {mModifyRequest = modReq} baseUrl =
  mkClientEnv man {mModifyRequest = modReq'} baseUrl
  where
    modReq' :: Request -> IO Request
    modReq' r = do
      x <- modReq r
      pure $
        if ((hContentEncoding, "gzip") `elem` requestHeaders x)
          then x
          else
            let new_hdrs = (hContentEncoding, "gzip") : requestHeaders x
                (hrds, body) = case requestBody x of
                  RequestBodyBuilder _ _ -> (requestHeaders x, requestBody x)
                  RequestBodyStream _ _ -> (requestHeaders x, requestBody x)
                  RequestBodyStreamChunked _ -> (requestHeaders x, requestBody x)
                  b -> (new_hdrs, compressBody b)
             in x {requestHeaders = hrds, requestBody = body}

    compressBody :: RequestBody -> RequestBody
    compressBody = \case
      RequestBodyLBS bsl -> RequestBodyLBS $ compress bsl
      RequestBodyBS bs -> RequestBodyLBS $ compress $ BSL.fromStrict bs
      RequestBodyIO iob -> RequestBodyIO $ compressBody <$> iob
      b -> b

api :: Proxy (VersionControlAPI a g)
api = Proxy

infernoVcClient :: (FromJSON a, FromJSON g, ToJSON a, ToJSON g) => Client ClientM (VersionControlAPI a g)
infernoVcClient = client api

-- TODO Generate this block below using TH given a and g:

type ClientMWithVCStoreError a = TypedClientM VCServerError a

-- fetchFunction :: forall a g. VCObjectHash -> ClientMWithVCStoreError (VCMeta a g (Expr (Pinned VCObjectHash) (), TCScheme))
-- fetchFunctionsForGroups :: forall a g. Set g -> ClientMWithVCStoreError [VCMeta a g VCObjectHash]
-- fetchVCObject :: forall a g. VCObjectHash -> ClientMWithVCStoreError (VCMeta a g VCObject)
-- fetchVCObjectHistory :: forall a g. VCObjectHash -> ClientMWithVCStoreError [VCMeta a g VCObjectHash]
-- fetchVCObjects :: forall a g. [VCObjectHash] -> ClientMWithVCStoreError (Map.Map VCObjectHash (VCMeta a g VCObject))
-- fetchVCObjectClosureHashes :: VCObjectHash -> ClientMWithVCStoreError [VCObjectHash]
-- pushFunction :: forall a g. VCMeta a g (Expr (Pinned VCObjectHash) (), TCScheme) -> ClientMWithVCStoreError VCObjectHash
-- deleteAutosavedFunction :: VCObjectHash -> ClientMWithVCStoreError ()
-- deleteVCObjects :: VCObjectHash -> ClientMWithVCStoreError ()
-- fetchFunction
--   :<|> fetchFunctionsForGroups
--   :<|> fetchVCObject
--   :<|> fetchVCObjectHistory
--   :<|> fetchVCObjects
--   :<|> fetchVCObjectClosureHashes
--   :<|> pushFunction
--   :<|> deleteAutosavedFunction
--   :<|> deleteVCObjects = typedClient $ client api
