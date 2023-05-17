module Inferno.ML.Remote.Client
  ( runInference,
  )
where

import Data.Proxy (Proxy)
import Data.Text (Text)
import Inferno.ML.Remote.Types (InfernoMlRemoteAPI)
import Servant.Client (ClientM, client)

runInference :: Proxy InfernoMlRemoteAPI -> Text -> ClientM Text
runInference = client
