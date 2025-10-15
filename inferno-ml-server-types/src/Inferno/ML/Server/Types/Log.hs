{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Logging utility types and functions
module Inferno.ML.Server.Types.Log where

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Plow.Logging.Async (withAsyncHandleTracer)
import Plow.Logging (IOTracer)
import Plow.Logging.Message
  ( LogLevel (LevelError, LevelWarn),
  )
import UnliftIO (MonadUnliftIO)
import UnliftIO.IO (Handle)

-- | A single logging message
data Message = Message LogLevel (StdStream Text)
  deriving stock (Show, Eq, Generic)

-- | Standard output streams
data StdStream a
  = Stdout a
  | Stderr a
  deriving stock (Show, Eq, Generic)

-- | Print a message, with the @Right@ case being @Stderr@ and the @Left@
-- @Stdout@. Can be then used with @withEitherTracer@ from @plow-logging@
printMessage :: Message -> Either Text Text
printMessage (Message level stream) = case stream of
  Stderr t -> printWithLevel Right t
  Stdout t
    | level `elem` [LevelWarn, LevelError] -> printWithLevel Right t
    | otherwise -> printWithLevel Left t
  where
    printWithLevel ::
      (Text -> Either Text Text) -> Text -> Either Text Text
    printWithLevel ctor = ctor . (mconcat ["[", Text.pack $ show level, "] "] <>)

-- | Create two async handle tracers
withAsyncHandleIOTracers ::
  forall m a.
  (MonadUnliftIO m) =>
  -- | @Stdout@ tracer
  Handle ->
  -- | @Stderr@ tracer
  Handle ->
  (IOTracer Text -> IOTracer Text -> m a) ->
  m a
withAsyncHandleIOTracers h1 h2 g = withAsyncHandleTracer h1 100 inner
  where
    inner :: IOTracer Text -> m a
    inner = withAsyncHandleTracer h2 100 . g
