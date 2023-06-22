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
    InfernoMlRemoteEnv (InfernoMlRemoteEnv),
    InfernoMlRemoteM,
    Options,
    mkOptions,
  )
import Lens.Micro.Platform ((^.))
import Network.HTTP.Types (Status)
import Network.Wai (Request)
import Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    runSettings,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)
import Servant (Application, ServerT, hoistServer, serve)

main :: IO ()
main = runServer =<< mkOptions
  where
    runServer :: Options -> IO ()
    runServer options =
      withStdoutLogger $
        (`runSettings` infernoMlRemote mkEnv) . mkSettings
      where
        mkSettings :: (Request -> Status -> Maybe Integer -> IO ()) -> Settings
        mkSettings logger =
          defaultSettings
            & setPort (options ^. #port & fromIntegral)
            & setLogger logger

        mkEnv :: InfernoMlRemoteEnv
        mkEnv = InfernoMlRemoteEnv $ options ^. #modelCache

infernoMlRemote :: InfernoMlRemoteEnv -> Application
infernoMlRemote env = serve api $ hoistServer api (`runReaderT` env) server

api :: Proxy InfernoMlRemoteAPI
api = Proxy

server :: ServerT InfernoMlRemoteAPI InfernoMlRemoteM
server = runInferenceHandler
