module Inferno.ML.Remote.Server
  ( InfernoMlRemoteAPI,
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
import Inferno.Core (Interpreter, mkInferno)
import Inferno.ML.Module.Prelude (mlPrelude)
import Inferno.ML.Remote.Handler (runInferenceHandler)
import Inferno.ML.Remote.Types
  ( InfernoMlRemoteAPI,
    Env (Env),
    RemoteM,
    ModelStore (Paths, Postgres),
    ModelStoreOption (PathOption, PostgresOption),
    Options,
    mkOptions,
  )
import Inferno.ML.Types.Value (MlValue)
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
    runServer options =
      mkInferno mlPrelude >>= \interpreter -> case options ^. #modelStore of
        PathOption p ->
          run . infernoMlRemote interpreter . mkEnv $
            Paths p
        PostgresOption p ->
          withConnect p $
            run . infernoMlRemote interpreter . mkEnv . Postgres
      where
        mkEnv :: ModelStore -> Env
        mkEnv = Env $ view #modelCache options

        run :: Application -> IO ()
        run app = withStdoutLogger $ (`runSettings` app) . mkSettings

        mkSettings :: (Request -> Status -> Maybe Integer -> IO ()) -> Settings
        mkSettings logger =
          defaultSettings
            & setPort (options ^. #port & fromIntegral)
            & setLogger logger

infernoMlRemote :: Interpreter MlValue -> Env -> Application
infernoMlRemote interpreter env =
  serve api
    . hoistServer api (`toHandler` env)
    $ server interpreter
  where
    toHandler :: RemoteM a -> Env -> Handler a
    toHandler m = Handler . ExceptT . try . runReaderT m

api :: Proxy InfernoMlRemoteAPI
api = Proxy

server :: Interpreter MlValue -> ServerT InfernoMlRemoteAPI RemoteM
server = runInferenceHandler
