{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Inferno.ML.Server.Client.Bridge
  ( valueAtC,
    latestValueAndTimeBeforeC,
  )
where

import Data.Data (Proxy (Proxy))
import Data.Int (Int64)
import Inferno.ML.Server.Types
import Servant ((:<|>) (..))
import Servant.Client.Streaming (ClientM, client)
import Web.HttpApiData (ToHttpApiData)

valueAtC ::
  (ToHttpApiData p, ToHttpApiData t) => Int64 -> p -> t -> ClientM IValue
latestValueAndTimeBeforeC ::
  (ToHttpApiData p, ToHttpApiData t) => p -> t -> ClientM IValue
valueAtC :<|> latestValueAndTimeBeforeC = client api

api :: Proxy (BridgeAPI p t)
api = Proxy
