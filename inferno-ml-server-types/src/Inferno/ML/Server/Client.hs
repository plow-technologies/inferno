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

import Conduit (ConduitT)
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (Scientific)
import Inferno.ML.Server.Types
import Servant ((:<|>) ((:<|>)))
import Servant.Client.Streaming (ClientM, client)

statusC :: ClientM (Maybe ())
inferenceC ::
  forall uid gid.
  Id (InferenceParam uid gid) ->
  ClientM (ConduitT () (AsValue Scientific) IO ())
cancelC :: ClientM ()
registerBridgeC :: BridgeInfo -> ClientM ()
checkBridgeC :: ClientM (Maybe BridgeInfo)
statusC
  :<|> inferenceC
  :<|> cancelC
  :<|> registerBridgeC
  :<|> checkBridgeC =
    client api

api :: Proxy (InfernoMlServerAPI uid gid)
api = Proxy
