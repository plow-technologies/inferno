module Inferno.ML.Server.Client.Bridge
  ( valueAtC,
  )
where

import Data.Data (Proxy (Proxy))
import Data.Int (Int64)
import Inferno.ML.Server.Types
import Servant.Client.Streaming (ClientM, client)
import Web.HttpApiData (ToHttpApiData)

valueAtC ::
  ( ToHttpApiData p,
    ToHttpApiData t
  ) =>
  Int64 ->
  p ->
  t ->
  ClientM IValue
valueAtC = client api

api :: Proxy (BridgeAPI p t)
api = Proxy
