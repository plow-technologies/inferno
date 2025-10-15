{-# LANGUAGE OverloadedRecordDot #-}

-- | This is the executable that implements the 'PerServerAPI'. Note that this
-- runs as a separate service from @inferno-ml-server@ itself
module Configure (main) where

import Control.Monad.Catch (throwM)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.Text as Text
import Inferno.ML.Server.Client.PerServer (api)
import Inferno.ML.Server.Types
import Lens.Micro.Platform
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
    err500,
    errBody,
    hoistServer,
    serve,
    (:<|>) ((:<|>)),
  )
import System.FilePath ((</>))
import UnliftIO.Exception
  ( Exception (displayException),
    SomeException,
    catchAny,
    try,
  )
import UnliftIO.IO.File (writeBinaryFileDurableAtomic)

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
        toHandler m =
          Handler
            . ExceptT
            . try
            . flip catchAny toServantErr
            . runReaderT m

        -- Make sure that relevant exceptions appear in body of response,
        -- otherwise we'll get generic Warp "Something went wrong"
        toServantErr :: SomeException -> IO a
        toServantErr e =
          throwM $
            err500
              { errBody =
                  ByteString.Lazy.Char8.pack . displayException $
                    e
              }

    server :: ServerT PerServerAPI PerServerM
    server = setPerServerConfig :<|> getPerServerConfig

-- Handlers

setPerServerConfig :: PerServerConfig -> PerServerM ()
setPerServerConfig cfg = flip catchAny (throwM . CouldntSetConfig cfg) $ do
  -- FIXME Trace here for setting config
  --
  writeBinaryFileDurableAtomic perServerConfigPath asBinary
  where
    asBinary :: ByteString
    asBinary = ByteString.Lazy.Char8.toStrict $ encode cfg

getPerServerConfig :: PerServerM PerServerConfig
getPerServerConfig = undefined

type PerServerM = ReaderT PerServerEnv IO

-- FIXME Dummy for now, add (at least) tracers later
newtype PerServerEnv = PerServerEnv ()

data PerServerError
  = CouldntSetConfig PerServerConfig SomeException
  deriving stock (Show)

instance Exception PerServerError where
  displayException = \case
    CouldntSetConfig cfg e ->
      unwords
        [ "Setting per-server config for instance"
        , Text.unpack cfg.instanceId
        , "failed" <> ","
        , "original exception:"
        , displayException e
        ]

perServerConfigPath :: FilePath
perServerConfigPath = infernoMlStateDirectory </> "per-server-config.json"
