module Inferno.ML.Remote.Client
  ( runInference,
  )
where

import Data.Proxy (Proxy)
import Inferno.ML.Remote.Types
  ( InferenceRequest,
    InferenceResponse,
    InfernoMlRemoteAPI,
  )
import Servant.Client (ClientM, client)

runInference ::
  Proxy InfernoMlRemoteAPI ->
  InferenceRequest ->
  ClientM InferenceResponse
runInference = client
