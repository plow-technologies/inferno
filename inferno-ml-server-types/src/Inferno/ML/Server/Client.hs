{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Client
  ( statusC,
    inferenceC,
    cancelC,
    registerBridgeC,
    checkBridgeC,
  )
where

import Data.Int (Int64)
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (Scientific)
import Inferno.ML.Server.Types
import Servant ((:<|>) ((:<|>)))
import Servant.Client.Streaming (ClientM, client)

statusC :: ClientM (Maybe ())
inferenceC ::
  Id (InferenceParam uid gid p s) ->
  Maybe Int64 ->
  ClientM (TStream Scientific IO)
cancelC :: ClientM ()
registerBridgeC :: BridgeInfo -> ClientM ()
checkBridgeC :: ClientM (Maybe BridgeInfo)
statusC
  :<|> inferenceC
  :<|> cancelC
  :<|> registerBridgeC
  :<|> checkBridgeC =
    client api

api :: Proxy (InfernoMlServerAPI uid gid p s)
api = Proxy
