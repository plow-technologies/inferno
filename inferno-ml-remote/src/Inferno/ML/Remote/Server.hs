module Inferno.ML.Remote.Server
  ( InfernoMlRemoteAPI,
    api,
    main,
    infernoMlRemote,
  )
where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Proxy (Proxy (Proxy))
import Inferno.ML.Remote.Handler (runInferenceHandler)
import Inferno.ML.Remote.Types
  ( InfernoMlRemoteAPI,
    InfernoMlRemoteEnv,
    InfernoMlRemoteM,
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
import Servant (Application, ServerT, hoistServer, serve)

main :: IO ()
main = runServer =<< parseOptions
  where
    runServer :: Options -> IO ()
    runServer options =
      withStdoutLogger $
        (`runSettings` infernoMlRemote undefined) . mkSettings
      where
        mkSettings logger =
          defaultSettings
            & setPort (options ^. #port & fromIntegral)
            & setLogger logger

infernoMlRemote :: InfernoMlRemoteEnv -> Application
infernoMlRemote env = serve api $ hoistServer api (flip runReaderT env) server

api :: Proxy InfernoMlRemoteAPI
api = Proxy

server :: ServerT InfernoMlRemoteAPI InfernoMlRemoteM
server = runInferenceHandler
