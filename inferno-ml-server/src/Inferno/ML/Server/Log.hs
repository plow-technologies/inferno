{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Log where

import Control.Exception (displayException)
import Data.Functor.Contravariant (contramap)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Inferno.ML.Server.Types
import Lens.Micro.Platform
import Plow.Logging (IOTracer (IOTracer), withEitherTracer)
import Plow.Logging.Async (withAsyncHandleTracer)
import Plow.Logging.Message
  ( LogLevel (LevelError, LevelInfo, LevelWarn),
  )
import UnliftIO (MonadUnliftIO)
import UnliftIO.IO (Handle, stderr, stdout)

traceRemote :: RemoteTrace -> Message
traceRemote = \case
  StartingServer -> info "Starting `inferno-ml-server`"
  RunningInference ipid t ->
    info $
      Text.unwords
        [ "Running inference param:",
          tshow ipid <> ",",
          "with timeout:",
          tshow $ t `div` 1000000,
          "(seconds)"
        ]
  EvaluatingScript i ->
    info $
      Text.unwords
        [ "Evaluating inferno script for parameter:",
          tshow i
        ]
  CancelingInference i ->
    warn $
      Text.unwords
        [ "Canceling inference job for param:",
          tshow i
        ]
  CopyingModel i -> info $ Text.unwords ["Copying model to cache:", tshow i]
  RegisteringBridge bi ->
    info $
      Text.unwords
        [ "Registering orchestrator bridge with IP address",
          bi ^. #host & tshow,
          "and port",
          bi ^. #port & tshow
        ]
  RemoteError e -> err . Text.pack $ displayException e
  OtherInfo t -> info t
  OtherWarn t -> warn t
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
  MonadUnliftIO m =>
  (IOTracer RemoteTrace -> m a) ->
  m a
withRemoteTracer f = withAsyncHandleIOTracers stdout stderr $
  \tso tse -> f $ mkRemoteTracer tso tse
  where
    withAsyncHandleIOTracers ::
      MonadUnliftIO m =>
      Handle ->
      Handle ->
      (IOTracer Text -> IOTracer Text -> m a) ->
      m a
    withAsyncHandleIOTracers h1 h2 g = withAsyncHandleTracer h1 100 inner
      where
        inner :: IOTracer Text -> m a
        inner = withAsyncHandleTracer h2 100 . g

    mkRemoteTracer ::
      IOTracer Text -> IOTracer Text -> IOTracer RemoteTrace
    mkRemoteTracer (IOTracer traceStdout) (IOTracer traceStderr) =
      IOTracer $
        contramap (printMessage . traceRemote) $
          withEitherTracer traceStdout traceStderr
      where
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