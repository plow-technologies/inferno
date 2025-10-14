-- | This is the executable that implements the 'PerServerAPI'. Note that this
-- runs as a separate service from @inferno-ml-server@ itself
module Configure (main) where

import Control.Monad.Except (ExceptT (ExceptT))
import Inferno.ML.Server.Client.PerServer (api)
import Inferno.ML.Server.Types
import Network.HTTP.Types (Status)
import Network.Wai (Request)
import Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    runSettings,
    setLogger,
    setPort,
  )
import Lens.Micro.Platform
import Network.Wai.Logger (withStdoutLogger)
import Servant
  ( Application,
    Handler (Handler),
    ServerT,
    hoistServer,
    serve,
    (:<|>) ((:<|>)),
  )
import System.FilePath ((</>))
import UnliftIO.Exception (try)
import Control.Monad.Reader (ReaderT (runReaderT))

main :: IO ()
main = runServer
  where
    runServer :: IO ()
    runServer = withEnv $ run . configureServer
      where
        run :: Application -> IO ()
        run app = withStdoutLogger $ (`runSettings` app) . mkSettings

    mkSettings :: (Request -> Status -> Maybe Integer -> IO ()) -> Settings
    mkSettings logger =
      defaultSettings
        & setPort 8081
        & setLogger logger

    withEnv :: (PerServerEnv -> IO ()) -> IO ()
    withEnv f = f $ PerServerEnv ()

    configureServer :: PerServerEnv -> Application
    configureServer env = serve api $ hoistServer api (`toHandler` env) server
      where
        toHandler :: PerServerM a -> PerServerEnv -> Handler a
        toHandler m = Handler . ExceptT . try . runReaderT m

    server :: ServerT PerServerAPI PerServerM
    server = setPerServerConfig :<|> getPerServerConfig

setPerServerConfig :: PerServerConfig -> PerServerM ()
setPerServerConfig = undefined

getPerServerConfig :: PerServerM PerServerConfig
getPerServerConfig = undefined

type PerServerM = ReaderT PerServerEnv IO

-- FIXME Dummy for now, add (at least) tracers later
newtype PerServerEnv = PerServerEnv ()

perServerConfigPath :: FilePath
perServerConfigPath = infernoMlStateDirectory </> "per-server-config.json"
