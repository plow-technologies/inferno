{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Proxy (Proxy (Proxy))
import Inferno.ML.Server.Inference
import Inferno.ML.Server.Log
import Inferno.ML.Server.Types
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
import Plow.Logging (IOTracer, traceWith)
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
            & setPort (fromIntegral cfg.port)
            & setLogger logger

runInEnv :: Config -> (Env -> IO ()) -> IO ()
runInEnv cfg f =
  withConnectionPool cfg.store $ \pool ->
    withRemoteTracer (cfg ^. #instanceId) pool $ \tracer -> do
      traceWith tracer $ InfoTrace StartingServer
      f
        =<< Env cfg tracer pool
          <$> newMVar ()
          <*> newEmptyMVar
          <*> newManager defaultManagerSettings
          <*> newIORef Nothing

infernoMlRemote :: Env -> Application
infernoMlRemote env = serve api $ hoistServer api (`toHandler` env) server
  where
    toHandler :: RemoteM a -> Env -> Handler a
    toHandler m =
      Handler
        . ExceptT
        . try
        . handle toServantErr
        . handle (logAndRethrowError (env ^. #tracer))
        . runReaderT m

    logAndRethrowError :: IOTracer RemoteTrace -> RemoteError -> IO a
    logAndRethrowError tracer err =
      traceWith tracer (ErrorTrace err) >> throwM err

    toServantErr :: RemoteError -> IO a
    toServantErr = throwM . translateError
      where
        errWith :: ServerError -> RemoteError -> ServerError
        errWith se e =
          se
            { errBody =
                ByteString.Lazy.Char8.pack . displayException $
                  e
            }

        translateError :: RemoteError -> ServerError
        translateError =
          \case
            e@OtherRemoteError{} -> errWith err500 e
            e@CacheSizeExceeded{} -> errWith err400 e
            e@NoSuchModel{} -> errWith err404 e
            e@NoSuchParameter{} -> errWith err404 e
            e@NoSuchScript{} -> errWith err404 e
            e@InvalidScript{} -> errWith err400 e
            e@InvalidOutput{} -> errWith err400 e
            e@InfernoError{} -> errWith err500 e
            e@NoBridgeSaved{} -> errWith err500 e
            e@ScriptTimeout{} -> errWith err500 e
            e@DbError{} -> errWith err500 e
            e@ClientError{} -> errWith err500 e

api :: Proxy InfernoMlServerAPI
api = Proxy

server :: ServerT InfernoMlServerAPI RemoteM
server =
  getStatus
    :<|> runInferenceParam
    :<|> testInferenceParam
    :<|> cancelInference
  where
    -- If the server is currently evaluating a script, the var will be taken,
    -- i.e. evaluate to `Nothing`, otherwise `Just ()`
    getStatus :: RemoteM ServerStatus
    getStatus =
      fmap (maybe EvaluatingScript (const Idle)) $
        tryReadMVar =<< view #lock

    -- When an inference request is run, the server will store the `Async` in
    -- the `job` `MVar`. Canceling the request throws to the `Async` thread
    cancelInference :: RemoteM ()
    cancelInference =
      maybe (pure ()) logAndCancel
        =<< tryTakeMVar
        =<< view #job
      where
        logAndCancel ::
          (Id InferenceParam, Async (Maybe (WriteStream IO))) -> RemoteM ()
        logAndCancel (i, j) = logWarn (CancelingInference i) *> cancel j
