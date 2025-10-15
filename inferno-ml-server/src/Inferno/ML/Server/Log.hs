{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Log where

import Control.Monad (when)
import Control.Monad.Reader (runReaderT)
import Data.Functor.Contravariant (contramap)
import Data.Pool (Pool)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Inferno.ML.Server.Types
import Inferno.ML.Server.Types.Log
import Inferno.ML.Server.Utils (executeStore)
import Plow.Logging
  ( IOTracer (IOTracer),
    Tracer (Tracer),
    withEitherTracer,
  )
import Plow.Logging.Message
  ( LogLevel (LevelError, LevelInfo, LevelWarn),
  )
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)
import UnliftIO.IO (stderr, stdout)

traceRemote :: RemoteTrace -> Message
traceRemote = \case
  i@InfoTrace{} -> info $ showTrace i
  w@WarnTrace{} -> warn $ showTrace w
  e@ErrorTrace{} -> err $ showTrace e
  where
    info, err, warn :: Text -> Message
    info = Message LevelInfo . Stdout
    err = Message LevelError . Stderr
    warn = Message LevelWarn . Stderr

withRemoteTracer ::
  forall m a.
  (MonadUnliftIO m) =>
  -- | Instance ID of the @inferno-ml-server@ instance
  Text ->
  Pool Connection ->
  (IOTracer RemoteTrace -> m a) ->
  m a
withRemoteTracer instanceId pool f = withAsyncHandleIOTracers stdout stderr $
  \tso tse ->
    f $ mkRemoteTracer tso tse
  where
    mkRemoteTracer :: IOTracer Text -> IOTracer Text -> IOTracer RemoteTrace
    mkRemoteTracer (IOTracer traceStdout) (IOTracer traceStderr) =
      IOTracer $ consoleTracer <> databaseTracer
      where
        -- Traces to the DB with the instance ID of the `inferno-ml-server`
        -- instance
        databaseTracer :: forall m'. (MonadIO m') => Tracer m' RemoteTrace
        databaseTracer = Tracer $ \t ->
          when (shallPersist t) . liftIO . flip runReaderT pool $
            executeStore
              [sql|INSERT INTO traces VALUES (?, now(), ?)|]
              (instanceId, t)

        -- Prints traces directly to stdout/stderr
        consoleTracer :: forall m'. (MonadIO m') => Tracer m' RemoteTrace
        consoleTracer =
          contramap (printMessage . traceRemote) $
            withEitherTracer traceStdout traceStderr

        -- We want essentially the entire process traced so that the traces
        -- can be recovered later and make the inference script evaluation process
        -- clear to the end user
        shallPersist :: RemoteTrace -> Bool
        shallPersist = \case
          InfoTrace info -> case info of
            -- This one is not necessary for the user
            CopyingModel{} -> False
            StartingServer{} -> True
            RunningInference{} -> True
            EvaluatingParam{} -> True
            OtherInfo{} -> True
            ExternalTrace{} -> True
          -- Having `LevelWarn` traces show up helps with debugging; the
          -- server does not generate many of these, so it shouldn't overwhelm
          -- the DB with garbage messages (unlike `LevelInfo`)
          WarnTrace warn -> case warn of
            CouldntMoveTensor{} -> True
            OomKilled{} -> True
            OtherWarn{} -> True
            CantMonitorMemory{} -> True
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
