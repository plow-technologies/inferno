module Inferno.ML.Remote.Server
  ( InfernoMlRemoteAPI,
    InfernoMlRemoteServer,
    infernoMlRemote,
    infernoMlRemoteAPI,
  )
where

import Data.Proxy (Proxy (Proxy))
import Inferno.ML.Remote.Handler (runInferenceHandler)
import Inferno.ML.Remote.Types (InfernoMlRemoteAPI, InfernoMlRemoteServer)
import Servant (Application, serve)

infernoMlRemote :: Application
infernoMlRemote = serve infernoMlRemoteAPI server

infernoMlRemoteAPI :: Proxy InfernoMlRemoteAPI
infernoMlRemoteAPI = Proxy

server :: InfernoMlRemoteServer
server = runInferenceHandler
