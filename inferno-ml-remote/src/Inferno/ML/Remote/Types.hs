{-# LANGUAGE TypeOperators #-}

module Inferno.ML.Remote.Types
  ( API,
    infernoMlRemoteAPI,
  )
where

import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant (JSON, Post, ReqBody, (:>))

-- TODO
-- Use more descriptive types. Implied `Text -> Text` is pretty awful
type API = "inference" :> ReqBody '[JSON] Text :> Post '[JSON] Text

infernoMlRemoteAPI :: Proxy API
infernoMlRemoteAPI = Proxy
