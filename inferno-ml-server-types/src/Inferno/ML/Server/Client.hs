{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Client where

import Data.Proxy (Proxy)
import Inferno.ML.Server.Types
import Servant.Client (ClientM, client)

runInference ::
  forall uid gid.
  Proxy (InfernoMlServerAPI uid gid) ->
  InferenceRequest uid gid ->
  ClientM InferenceResponse
runInference = client
