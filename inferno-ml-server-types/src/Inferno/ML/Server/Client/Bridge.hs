{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Inferno.ML.Server.Client.Bridge where

import Data.Aeson (ToJSON)
import Data.Data (Proxy (Proxy))
import Data.Int (Int64)
import Inferno.ML.Server.Types
import Servant ((:<|>) (..))
import Servant.Client.Streaming (ClientM, client)
import Web.HttpApiData (ToHttpApiData)

-- | Write a stream of @(t, Double)@ pairs to the bridge server, where @t@ will
-- typically represent some time value (e.g. @EpochTime@)
writePairsC ::
  ( ToJSON t,
    ToHttpApiData p,
    ToHttpApiData t
  ) =>
  p ->
  PairStream t IO ->
  ClientM ()

-- | Get the value at the given time via the bridge, for the given entity @p@
valueAtC ::
  ( ToJSON t,
    ToHttpApiData p,
    ToHttpApiData t
  ) =>
  Int64 ->
  p ->
  t ->
  ClientM IValue

-- | Get the latest value and the time
latestValueAndTimeBeforeC ::
  ( ToJSON t,
    ToHttpApiData p,
    ToHttpApiData t
  ) =>
  t ->
  p ->
  ClientM IValue
writePairsC
  :<|> valueAtC
  :<|> latestValueAndTimeBeforeC =
    client api

api :: Proxy (BridgeAPI p t)
api = Proxy
