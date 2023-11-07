{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Client where

import Conduit (ConduitT)
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (Scientific)
import Inferno.ML.Server.Types
import Servant.Client.Streaming (ClientM, client)

runInference ::
  forall uid gid.
  InferenceRequest uid gid ->
  ClientM (ConduitT () (AsValue Scientific) IO ())
runInference = client api

api :: Proxy (InfernoMlServerAPI uid gid)
api = Proxy
