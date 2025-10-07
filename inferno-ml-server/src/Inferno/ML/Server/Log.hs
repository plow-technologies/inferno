{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Log where

import Control.Exception (displayException)
import Control.Monad (when)
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor.Contravariant (contramap)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as T
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple
  ( Connection,
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)
import Inferno.ML.Server.Types
import Inferno.ML.Server.Utils (executeStore)
import qualified Network.HTTP.Client as HTTP
import Plow.Logging
  ( IOTracer (IOTracer),
    Tracer (Tracer),
    withEitherTracer,
  )
import Plow.Logging.Async (withAsyncHandleTracer)
import Plow.Logging.Message
  ( LogLevel (LevelError, LevelInfo, LevelWarn),
  )
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)
import UnliftIO.IO (Handle, stderr, stdout)

traceRemote :: RemoteTrace -> Message
traceRemote = \case
  InfoTrace i -> info $ case i of
    StartingServer -> "Starting `inferno-ml-server`"
    RunningInference ipid t ->
      Text.unwords
        [ "Running inference param:"
        , tshow ipid <> ","
        , "with timeout:"
        , tshow $ t `div` 1000000
        , "(seconds)"
        ]
    EvaluatingParam s ->
      Text.unwords
        [ "Evaluating inferno script for parameter:"
        , tshow s
        ]
    CopyingModel m ->
      Text.unwords
        [ "Copying model to cache:"
        , tshow m
        ]
    OtherInfo t -> t
  WarnTrace w -> warn $ case w of
    CancelingInference i ->
      Text.unwords
        [ "Canceling inference job for param:"
        , tshow i
        ]
    CouldntMoveTensor dev ->
      Text.pack $
        unwords
          [ "Couldn't move tensor to device"
          , dev
          ]
    OomKilled time ->
      Text.pack $
        unwords
          [ "Server is restarting from out-of-memory event"
          , "triggered at"
          , show time
          ]
    CantMonitorMemory details ->
      Text.unwords
        [ details <> ","
        , "cannot monitor memory usage;"
        , "running action without memory monitoring"
        ]
    OtherWarn t -> t
  ErrorTrace e -> err . Text.pack $ displayException e
  where
    info, err, warn :: Text -> Message
    info = Message LevelInfo . Stdout
    err = Message LevelError . Stderr
    warn = Message LevelWarn . Stderr

-- | A single logging message
data Message = Message LogLevel (StdStream Text)
  deriving stock (Show, Eq, Generic)

-- | Standard output streams
data StdStream a
  = Stdout a
  | Stderr a
  deriving stock (Show, Eq, Generic)

withRemoteTracer ::
  forall m a.
  (MonadUnliftIO m) =>
  InstanceId ->
  Pool Connection ->
  (IOTracer RemoteTrace -> m a) ->
  m a
withRemoteTracer instanceId pool f = withAsyncHandleIOTracers stdout stderr $
  \tso tse -> do
    mInstanceId <- case instanceId of
      InstanceId instanceId' -> pure $ Just instanceId'
      Auto -> Just <$> queryInstanceId
      NoDbLogging -> pure Nothing
    f $ mkRemoteTracer mInstanceId tso tse
  where
    withAsyncHandleIOTracers ::
      (MonadUnliftIO m) =>
      Handle ->
      Handle ->
      (IOTracer Text -> IOTracer Text -> m a) ->
      m a
    withAsyncHandleIOTracers h1 h2 g = withAsyncHandleTracer h1 100 inner
      where
        inner :: IOTracer Text -> m a
        inner = withAsyncHandleTracer h2 100 . g

    mkRemoteTracer ::
      Maybe Text -> IOTracer Text -> IOTracer Text -> IOTracer RemoteTrace
    mkRemoteTracer mInstanceId (IOTracer traceStdout) (IOTracer traceStderr) =
      IOTracer $ consoleTracer <> maybe mempty databaseTracer mInstanceId
      where
        databaseTracer :: forall m'. (MonadIO m') => Text -> Tracer m' RemoteTrace
        databaseTracer instanceId' = Tracer $ \t ->
          when (shallPersist t) $
            liftIO $ do
              ts <- getCurrentTime
              flip runReaderT pool $
                executeStore
                  [sql|INSERT INTO traces (instance_id, ts, trace) VALUES (?, ?, ?)|]
                  (instanceId', ts, t)

        consoleTracer :: forall m'. (MonadIO m') => Tracer m' RemoteTrace
        consoleTracer =
          contramap (printMessage . traceRemote) $
            withEitherTracer traceStdout traceStderr

        shallPersist :: RemoteTrace -> Bool
        shallPersist = \case
          InfoTrace _ -> False
          -- Having `LevelWarn` traces show up helps with debugging; the
          -- server does not generate many of these, so it shouldn't overwhelm
          -- the DB with garbage messages (unlike `LevelInfo`)
          WarnTrace warn -> case warn of
            CouldntMoveTensor{} -> True
            OomKilled{} -> True
            OtherWarn{} -> True
            -- This one is not really necessary for debugging
            CancelingInference{} -> False
          ErrorTrace err -> case err of
            CacheSizeExceeded -> False
            NoSuchModel{} -> True
            NoSuchScript{} -> True
            NoSuchParameter{} -> True
            InvalidScript{} -> True
            InvalidOutput{} -> True
            InfernoError{} -> True
            NoBridgeSaved{} -> True
            ScriptTimeout{} -> True
            MemoryLimitExceeded{} -> True
            DbError{} -> True
            ClientError{} -> True
            OtherRemoteError{} -> True

        printMessage :: Message -> Either Text Text
        printMessage (Message level stream) = case stream of
          Stderr t -> printWithLevel Right t
          Stdout t
            | level `elem` [LevelWarn, LevelError] -> printWithLevel Right t
            | otherwise -> printWithLevel Left t
          where
            printWithLevel ::
              (Text -> Either Text Text) -> Text -> Either Text Text
            printWithLevel ctor = ctor . (mconcat ["[", tshow level, "] "] <>)

-- | Retrieve EC2 instance id from EC2 environment. See
-- https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instancedata-data-retrieval.html
queryInstanceId :: (MonadIO m) => m Text
queryInstanceId = liftIO $ do
  req <- HTTP.parseRequest "http://169.254.169.254/latest/meta-data/instance-id"
  mgr <- HTTP.newManager HTTP.defaultManagerSettings
  T.decodeUtf8Lenient . LBS.toStrict . HTTP.responseBody <$> HTTP.httpLbs req mgr
