{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Client where

import Conduit (ConduitT)
import Data.Proxy (Proxy)
import Inferno.ML.Server.Types
import Servant.Client.Streaming (ClientM, client)

runInference ::
  forall uid gid.
  Proxy (InfernoMlServerAPI uid gid) ->
  InferenceRequest uid gid ->
  ClientM (ConduitT () SomeChunk IO ())
runInference = client
