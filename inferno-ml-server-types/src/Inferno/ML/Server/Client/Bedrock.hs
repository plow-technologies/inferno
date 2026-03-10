module Inferno.ML.Server.Client.Bedrock
  ( promptC,
  )
where

import Data.Data (Proxy (Proxy))
import Inferno.ML.Server.Types
import Servant.Client.Streaming (ClientM, client)

promptC :: BedrockRequest gid p -> ClientM BedrockResult
promptC = client api

api :: Proxy (PromptAPI gid p)
api = Proxy
