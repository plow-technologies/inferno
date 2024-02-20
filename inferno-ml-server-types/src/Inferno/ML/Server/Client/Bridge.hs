{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Inferno.ML.Server.Client.Bridge where

import Data.Aeson (FromJSON)
import Data.Data (Proxy (Proxy))
import Data.Int (Int64)
import Inferno.ML.Server.Types
import Servant ((:<|>) (..))
import Servant.Client.Streaming (ClientM, client)
import Web.HttpApiData (ToHttpApiData)

valueAtC ::
  ( FromJSON t,
    ToHttpApiData p,
    ToHttpApiData t
  ) =>
  Int64 ->
  p ->
  t ->
  ClientM IValue
latestValueAndTimeBeforeC ::
  ( FromJSON t,
    ToHttpApiData p,
    ToHttpApiData t
  ) =>
  t ->
  p ->
  ClientM IValue
writePairsC ::
  ( FromJSON t,
    ToHttpApiData p,
    ToHttpApiData t
  ) =>
  p ->
  ClientM (PairStream t IO)
writePairsC
  :<|> valueAtC
  :<|> latestValueAndTimeBeforeC =
    client api

api :: Proxy (BridgeAPI p t)
api = Proxy
