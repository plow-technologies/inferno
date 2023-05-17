{-# LANGUAGE TypeOperators #-}

module Inferno.ML.Remote.Types
  ( InfernoMlRemoteAPI,
    infernoMlRemoteAPI,
  )
where

import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant (JSON, Post, ReqBody, (:>))

-- TODO
-- Use more descriptive types. Implied `Text -> Text` is pretty awful
type InfernoMlRemoteAPI = "inference" :> ReqBody '[JSON] Text :> Post '[JSON] Text

infernoMlRemoteAPI :: Proxy InfernoMlRemoteAPI
infernoMlRemoteAPI = Proxy
