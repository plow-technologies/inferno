module Inferno.ML.Remote.Server
  ( InfernoMlRemoteAPI,
    runInEnv,
    api,
    main,
    infernoMlRemote,
  )
where

import Control.Exception (try)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Proxy (Proxy (Proxy))
import Database.PostgreSQL.Simple (withConnect)
import Inferno.Core (mkInferno)
import Inferno.ML.Module.Prelude (mlPrelude)
import Inferno.ML.Remote.Handler (runInferenceHandler)
import Inferno.ML.Remote.Types
  ( Env (Env),
    InfernoMlRemoteAPI,
    Options,
    RemoteM,
    mkOptions,
  )
import Lens.Micro.Platform (view, (^.))
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
import Servant
  ( Application,
    Handler (Handler),
    ServerT,
    hoistServer,
    serve,
  )

main :: IO ()
main = runServer =<< mkOptions
  where
    runServer :: Options -> IO ()
    runServer options = runInEnv options $ run . infernoMlRemote
      where
        run :: Application -> IO ()
        run app = withStdoutLogger $ (`runSettings` app) . mkSettings

        mkSettings :: (Request -> Status -> Maybe Integer -> IO ()) -> Settings
        mkSettings logger =
          defaultSettings
            & setPort (options ^. #port & fromIntegral)
            & setLogger logger

runInEnv :: Options -> (Env -> IO ()) -> IO ()
runInEnv options f = withConnect (options ^. #store) $ \conn ->
  f . Env (view #cache options) conn
    =<< mkInferno mlPrelude

infernoMlRemote :: Env -> Application
infernoMlRemote env = serve api $ hoistServer api (`toHandler` env) server
  where
    toHandler :: RemoteM a -> Env -> Handler a
    toHandler m = Handler . ExceptT . try . runReaderT m

api :: Proxy InfernoMlRemoteAPI
api = Proxy

server :: ServerT InfernoMlRemoteAPI RemoteM
server = runInferenceHandler
