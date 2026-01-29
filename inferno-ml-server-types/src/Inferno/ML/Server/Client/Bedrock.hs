module Inferno.ML.Server.Client.Bedrock
  ( promptC,
  )
where

import Data.Data (Proxy (Proxy))
import Inferno.ML.Server.Types
import Servant.Client.Streaming (ClientM, client)

promptC :: BedrockRequest -> ClientM BedrockResult
promptC = client api

api :: Proxy PromptAPI
api = Proxy
