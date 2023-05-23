module Inferno.ML.Remote.Server
  ( InfernoMlRemoteAPI,
    InfernoMlRemoteServer,
    main,
    infernoMlRemote,
    infernoMlRemoteAPI,
  )
where

import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Proxy (Proxy (Proxy))
import Inferno.ML.Remote.Handler (runInferenceHandler)
import Inferno.ML.Remote.Types
  ( InfernoMlRemoteAPI,
    InfernoMlRemoteServer,
    Options,
    parseOptions,
  )
import Lens.Micro.Platform ((^.))
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)
import Servant (Application, serve)

main :: IO ()
main = runServer =<< parseOptions
  where
    runServer :: Options -> IO ()
    runServer options =
      withStdoutLogger $
        (`runSettings` infernoMlRemote) . mkSettings
      where
        mkSettings logger =
          defaultSettings
            & setPort (options ^. #port & fromIntegral)
            & setLogger logger

infernoMlRemote :: Application
infernoMlRemote = serve infernoMlRemoteAPI server

infernoMlRemoteAPI :: Proxy InfernoMlRemoteAPI
infernoMlRemoteAPI = Proxy

server :: InfernoMlRemoteServer
server = runInferenceHandler
