-- |
module Inferno.ML.Remote.Types
  ( InfernoMlRemoteAPI,
    InfernoMlRemoteServer
  )
where

import Data.Text (Text)
import Servant (JSON, Post, ReqBody, Server, (:>))

-- TODO
-- Use more descriptive types. Implied `Text -> Text` is pretty awful
type InfernoMlRemoteAPI = "inference" :> ReqBody '[JSON] Text :> Post '[JSON] Text

type InfernoMlRemoteServer = Server InfernoMlRemoteAPI
