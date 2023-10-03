module Inferno.ML.Remote.Server
  ( InfernoMlRemoteAPI,
    api,
    main,
    infernoMlRemote,
  )
where

import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Proxy (Proxy (Proxy))
import Inferno.Core (Interpreter, mkInferno)
import Inferno.ML.Module.Prelude (mlPrelude)
import Inferno.ML.Remote.Handler (runInferenceHandler)
import Inferno.ML.Remote.Types
  ( InfernoMlRemoteAPI,
    InfernoMlRemoteEnv (InfernoMlRemoteEnv),
    InfernoMlRemoteM,
    Options,
    mkOptions,
  )
import Inferno.ML.Types.Value (MlValue)
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
    runServer options = do
      interpreter <- liftIO $ mkInferno mlPrelude
      withStdoutLogger $
        (`runSettings` infernoMlRemote interpreter mkEnv) . mkSettings
      where
        mkSettings :: (Request -> Status -> Maybe Integer -> IO ()) -> Settings
        mkSettings logger =
          defaultSettings
            & setPort (options ^. #port & fromIntegral)
            & setLogger logger

        mkEnv :: InfernoMlRemoteEnv
        mkEnv = InfernoMlRemoteEnv $ options ^. #modelCache

infernoMlRemote :: Interpreter IO MlValue -> InfernoMlRemoteEnv -> Application
infernoMlRemote interpreter env =
  serve api $ hoistServer api (`runReaderT` env) (server interpreter)

api :: Proxy InfernoMlRemoteAPI
api = Proxy

server :: Interpreter IO MlValue -> ServerT InfernoMlRemoteAPI InfernoMlRemoteM
server interpreter =
  runInferenceHandler interpreter
