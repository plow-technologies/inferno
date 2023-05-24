module Inferno.ML.Remote.Client
  ( runInference,
  )
where

import Data.Proxy (Proxy)
import Inferno.ML.Remote.Types (EvalResult, InfernoMlRemoteAPI, Script)
import Servant.Client (ClientM, client)

runInference :: Proxy InfernoMlRemoteAPI -> Script -> ClientM EvalResult
runInference = client
