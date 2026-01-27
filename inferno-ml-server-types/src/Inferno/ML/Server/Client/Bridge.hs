{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Inferno.ML.Server.Client.Bridge where

import Data.Aeson (ToJSON)
import Data.Data (Proxy (Proxy))
import Data.Int (Int64)
import Inferno.ML.Server.Types
import Servant ((:<|>) ((:<|>)))
import Servant.Client.Streaming (ClientM, client)
import Web.HttpApiData (ToHttpApiData)

-- | Get the value at the given time via the bridge, for the given entity @p@
valueAtC ::
  ( ToJSON t
  , ToHttpApiData p
  , ToHttpApiData t
  ) =>
  Int64 ->
  p ->
  t ->
  ClientM IValue

-- | Get the latest value (and associated time) for entity @p@ before time @t@
latestValueAndTimeBeforeC ::
  ( ToJSON t
  , ToHttpApiData p
  , ToHttpApiData t
  ) =>
  t ->
  p ->
  ClientM IValue

-- | Get an array of values falling between the two times
valuesBetweenC ::
  (ToHttpApiData p, ToHttpApiData t) => Int64 -> p -> t -> t -> ClientM IValue

valueAtC
  :<|> latestValueAndTimeBeforeC
  :<|> valuesBetweenC =
    client api

api :: Proxy (BridgeAPI p t)
api = Proxy
