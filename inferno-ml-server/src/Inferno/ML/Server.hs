{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server
  ( runInEnv,
    api,
    main,
    infernoMlRemote,
  )
where

import Control.Monad.Catch (throwM)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (decodeFileStrict)
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Proxy (Proxy (Proxy))
import Database.PostgreSQL.Simple (withConnect)
import Inferno.ML.Server.Bridge
import qualified Inferno.ML.Server.Client.Bridge as Bridge
import Inferno.ML.Server.Inference
import Inferno.ML.Server.Log
import Inferno.ML.Server.Types
import Inferno.ML.Server.Utils (bridgeCache)
import Lens.Micro.Platform
import Network.HTTP.Client
  ( defaultManagerSettings,
    newManager,
  )
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
import Plow.Logging (traceWith)
import Servant
  ( Application,
    Handler (Handler),
    ServerError (errBody),
    ServerT,
    err400,
    err404,
    err500,
    hoistServer,
    serve,
    (:<|>) ((:<|>)),
  )
import UnliftIO.Async (Async, cancel)
import UnliftIO.Directory (doesFileExist)
import UnliftIO.Exception
  ( Exception (displayException),
    handle,
    try,
  )
import UnliftIO.IORef (newIORef)
import UnliftIO.MVar
  ( newEmptyMVar,
    newMVar,
    tryReadMVar,
    tryTakeMVar,
  )

main :: IO ()
main = runServer =<< mkOptions
  where
    runServer :: Config -> IO ()
    runServer cfg = runInEnv cfg $ run . infernoMlRemote
      where
        run :: Application -> IO ()
        run app = withStdoutLogger $ (`runSettings` app) . mkSettings

        mkSettings :: (Request -> Status -> Maybe Integer -> IO ()) -> Settings
        mkSettings logger =
          defaultSettings
            & setPort (view #port cfg & fromIntegral)
            & setLogger logger

runInEnv :: Config -> (Env -> IO ()) -> IO ()
runInEnv cfg f = withRemoteTracer $ \tracer -> do
  traceWith tracer StartingServer
  withConnect (view #store cfg) $ \conn ->
    f
      =<< Env cfg conn tracer
        <$> newMVar ()
        <*> newEmptyMVar
        <*> mkBridge
        <*> newManager defaultManagerSettings
        <*> newIORef Nothing
  where
    mkBridge :: IO Bridge
    mkBridge = Bridge defaultBridgeClient <$> (newIORef =<< maybeDecodeBridge)

    maybeDecodeBridge :: IO (Maybe BridgeInfo)
    maybeDecodeBridge =
      doesFileExist bridgeCache >>= \case
        False -> pure Nothing
        True -> decodeFileStrict bridgeCache

    defaultBridgeClient :: BridgeClient
    defaultBridgeClient =
      BridgeClient
        Bridge.valueAtC
        Bridge.latestValueAndTimeBeforeC
        Bridge.writePairsC

infernoMlRemote :: Env -> Application
infernoMlRemote env = serve api $ hoistServer api (`toHandler` env) server
  where
    toHandler :: RemoteM a -> Env -> Handler a
    toHandler m =
      Handler
        . ExceptT
        . try
        . handle toServantErr
        . runReaderT m

    toServantErr :: RemoteError -> IO a
    toServantErr =
      throwM . \case
        e@OtherRemoteError {} -> errWith err500 e
        e@CacheSizeExceeded {} -> errWith err400 e
        e@NoSuchModel {} -> errWith err404 e
        e@NoSuchParameter {} -> errWith err404 e
        e@NoSuchScript {} -> errWith err404 e
        e@InvalidScript {} -> errWith err400 e
        e@InfernoError {} -> errWith err500 e
        e@BridgeNotRegistered {} -> errWith err500 e
        e@ScriptTimeout {} -> errWith err500 e
        e@ClientError {} -> errWith err500 e
      where
        errWith :: ServerError -> RemoteError -> ServerError
        errWith se e =
          se
            { errBody =
                ByteString.Lazy.Char8.pack . displayException $
                  e
            }

api :: Proxy InfernoMlServerAPI
api = Proxy

server :: ServerT InfernoMlServerAPI RemoteM
server =
  getStatus
    :<|> runInferenceParam
    :<|> cancelInference
    :<|> registerBridgeInfo
    :<|> getBridgeInfo
  where
    -- If the server is currently evaluating a script, this will return `Nothing`,
    -- otherwise `Just ()`
    getStatus :: RemoteM (Maybe ())
    getStatus = tryReadMVar =<< view #lock

    -- When an inference request is run, the server will store the `Async` in
    -- the `job` `MVar`. Canceling the request throws to the `Async` thread
    cancelInference :: RemoteM ()
    cancelInference =
      maybe (pure ()) logAndCancel
        =<< tryTakeMVar
        =<< view #job
      where
        logAndCancel :: (Id InferenceParam, Async (Maybe ())) -> RemoteM ()
        logAndCancel (i, j) = logTrace (CancelingInference i) *> cancel j
