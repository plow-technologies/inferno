module Inferno.ML.Remote.Client
  ( runInference,
  )
where

import Data.Proxy (Proxy)
import Data.Text (Text)
import Inferno.ML.Remote.Types (API)
import Servant.Client (ClientM, client)

runInference :: Proxy API -> Text -> ClientM Text
runInference = client @API
