{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This is the executable that implements the 'PerServerAPI'. Note that this
-- runs as a separate service from @inferno-ml-server@ itself
module Configure (main) where

import Control.Monad.Catch (throwM)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (eitherDecodeFileStrict, encode)
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Functor.Contravariant (contramap)
import Data.Generics.Labels ()
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Inferno.ML.Server.Client.PerServer (api)
import Inferno.ML.Server.Types
import Inferno.ML.Server.Types.Log
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
import Plow.Logging
  ( IOTracer (IOTracer),
    traceWith,
    withEitherTracer,
  )
import Plow.Logging.Message
  ( LogLevel (LevelError, LevelInfo),
  )
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
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory (doesPathExist)
import UnliftIO.Exception
  ( Exception (displayException),
    SomeException,
    catchAny,
    try,
  )
import UnliftIO.IO (stderr, stdout)
import UnliftIO.IO.File (writeBinaryFileDurableAtomic)

main :: IO ()
main = withTracer runServer
  where
    runServer :: IOTracer Message -> IO ()
    runServer tracer = do
      traceWith tracer $ infoMsg "Starting `inferno-ml-configure`"
      withEnv tracer $ run . configureServer
      where
        run :: Application -> IO ()
        run app = withStdoutLogger $ (`runSettings` app) . mkSettings

    mkSettings :: (Request -> Status -> Maybe Integer -> IO ()) -> Settings
    mkSettings logger =
      defaultSettings
        & setPort 8081
        & setLogger logger

    withEnv :: IOTracer Message -> (PerServerEnv -> IO ()) -> IO ()
    withEnv tracer f = f $ PerServerEnv tracer

    configureServer :: PerServerEnv -> Application
    configureServer env = serve api $ hoistServer api (`toHandler` env) server
      where
        toHandler :: PerServerM a -> PerServerEnv -> Handler a
        toHandler m =
          Handler
            . ExceptT
            . try
            . flip catchAny logThenToServantErr
            . runReaderT m

        -- Make sure that relevant exceptions appear in body of response,
        -- otherwise we'll get generic Warp "Something went wrong"
        logThenToServantErr :: SomeException -> IO a
        logThenToServantErr e = do
          traceWith env.tracer $ exceptionMsg e
          -- All of the current exceptions can be treated as 500s for simplicity's
          -- sake
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
  trace $ infoMsg "Setting per-server configuration"
  writeBinaryFileDurableAtomic perServerConfigPath
    . ByteString.Lazy.Char8.toStrict
    $ encode cfg

getPerServerConfig :: PerServerM PerServerConfig
getPerServerConfig = do
  trace $ infoMsg "Attempting to get per-server configuration (if any)"
  doesPathExist perServerConfigPath >>= \case
    False -> throwM NoConfigSet
    True ->
      either (throwM . CouldntDecodeConfig) pure
        =<< liftIO (eitherDecodeFileStrict perServerConfigPath)

type PerServerM = ReaderT PerServerEnv IO

newtype PerServerEnv = PerServerEnv
  { tracer :: IOTracer Message
  }
  deriving stock (Generic)

data PerServerError
  = CouldntSetConfig PerServerConfig SomeException
  | NoConfigSet
  | CouldntDecodeConfig String
  deriving stock (Show, Generic)

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
    NoConfigSet -> "No per-server configuration file exists"
    CouldntDecodeConfig s ->
      unwords
        [ "Failed to decode per-server configuration file:"
        , s
        ]

withTracer ::
  forall m a.
  (MonadUnliftIO m) =>
  (IOTracer Message -> m a) ->
  m a
withTracer f = withAsyncHandleIOTracers stdout stderr $ \out err ->
  f $ mkTracer out err
  where
    mkTracer :: IOTracer Text -> IOTracer Text -> IOTracer Message
    mkTracer (IOTracer traceStdout) (IOTracer traceStderr) =
      IOTracer $
        contramap printMessage $
          withEitherTracer traceStdout traceStderr

trace :: Message -> PerServerM ()
trace msg = (`traceWith` msg) =<< view #tracer

infoMsg :: Text -> Message
infoMsg = Message LevelInfo . Stdout

exceptionMsg :: (Exception e) => e -> Message
exceptionMsg = Message LevelError . Stderr . Text.pack . displayException
