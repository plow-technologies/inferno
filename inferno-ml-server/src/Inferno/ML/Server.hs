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
import Control.Monad.Extra (whenJustM)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Time (UTCTime, getCurrentTime)
import Inferno.ML.Server.Inference
import Inferno.ML.Server.Log
import Inferno.ML.Server.Types
import Inferno.ML.Server.Utils
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
import Text.Read (readMaybe)
import UnliftIO.Async (Async, cancel)
import UnliftIO.Directory (doesPathExist, removeFile)
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
    runServer :: GlobalConfig -> IO ()
    runServer cfg = runInEnv cfg $ run . infernoMlRemote
      where
        run :: Application -> IO ()
        run app = withStdoutLogger $ (`runSettings` app) . mkSettings

        mkSettings :: (Request -> Status -> Maybe Integer -> IO ()) -> Settings
        mkSettings logger =
          defaultSettings
            & setPort (fromIntegral cfg.port)
            & setLogger logger

runInEnv :: GlobalConfig -> (Env -> IO ()) -> IO ()
runInEnv cfg f =
  withConnectionPool cfg.store $ \pool ->
    -- FIXME Add instance ID from per-server config
    withRemoteTracer undefined pool $ \tracer -> do
      traceWith tracer $ InfoTrace StartingServer
      whenJustM wasOomKilled $ traceWith tracer . WarnTrace . OomKilled
      f
        =<< Env cfg tracer pool
          <$> newMVar ()
          <*> newEmptyMVar
          <*> newManager defaultManagerSettings
          <*> getMemoryMax
          <*> newIORef mempty
          <*> newIORef Nothing
  where
    -- See if we are restarting from a recent OOM event, i.e. the systemd service
    -- manager stopped the service because it exceeded its `MemoryMax` and
    -- the `OOMPolicy` was invoked. This leaves a breadcrumb file in the state
    -- directory. If it exists, the time when OOM was invoked it returned and
    -- the file is removed
    wasOomKilled :: IO (Maybe UTCTime)
    wasOomKilled =
      doesPathExist lastOomPath >>= \case
        -- No `oom_kill` was triggered before this server start, so no warning
        False -> pure Nothing
        True -> do
          -- The contents of `/var/lib/inferno-ml-server/last-oom` is just
          -- the UTC time of when the system process was killed because of
          -- an OOM event
          contents <- Text.unpack . Text.strip <$> Text.IO.readFile lastOomPath
          -- Fall back to current time if the file contents can't be parsed
          -- as a `UTCTime`. The time is only used for logging as a convenience
          time <- maybe getCurrentTime pure $ readMaybe contents
          -- Remove the breadcrumb, otherwise we will have false positives
          -- when restarting the server next time if there's not another
          -- OOM kill event
          --
          -- This file is owned by the `inferno` user, which is the same user
          -- that the server is running as, so we don't need to worry about
          -- permissions, etc...
          Just time <$ removeFile lastOomPath

infernoMlRemote :: Env -> Application
infernoMlRemote env = serve api $ hoistServer api (`toHandler` env) server
  where
    toHandler :: RemoteM a -> Env -> Handler a
    toHandler m =
      Handler
        . ExceptT
        . try
        . handle toServantErr
        . handle (logAndRethrowError env.tracer)
        . runReaderT m

    logAndRethrowError :: IOTracer RemoteTrace -> RemoteError -> IO a
    logAndRethrowError tracer err = traceWith tracer (ErrorTrace err) *> throwM err

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
        translateError = \case
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
          e@MemoryLimitExceeded{} -> errWith err500 e
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
