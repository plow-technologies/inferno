{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Inferno.ML.Server.Client
  ( statusC,
    inferenceC,
    cancelC,
    registerBridgeC,
    checkBridgeC,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Proxy (Proxy (Proxy))
import Inferno.ML.Server.Types
import Servant (ToHttpApiData, (:<|>) ((:<|>)))
import Servant.Client.Streaming (ClientM, client)

-- | Get the status of the server. @Nothing@ indicates that an inference job
-- is being evaluated. @Just ()@ means the server is idle
statusC :: ClientM (Maybe ())

-- | Run an inference parameter
inferenceC ::
  ( FromJSON t,
    ToHttpApiData t
  ) =>
  Id (InferenceParam uid gid p s) ->
  Maybe Int64 ->
  ClientM (PairStream t IO)

-- | Cancel the existing inference job, if it exists
cancelC :: ClientM ()

-- | Register the information required to communicate with the bridge server
registerBridgeC :: BridgeInfo -> ClientM ()

-- | Check if any bridge information has been previously registered with this
-- server instance
checkBridgeC :: ClientM (Maybe BridgeInfo)
statusC
  :<|> inferenceC
  :<|> cancelC
  :<|> registerBridgeC
  :<|> checkBridgeC =
    -- client api
    -- TODO fix the type error
    undefined

api :: Proxy (InfernoMlServerAPI uid gid p s t)
api = Proxy
