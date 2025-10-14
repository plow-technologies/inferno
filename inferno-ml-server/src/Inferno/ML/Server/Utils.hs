{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Utils
  ( HasPool,
    throwInfernoError,
    throwRemoteError,
    firstOrThrow,
    queryStore,
    executeStore,
    withConns,
    withMemoryMonitor,
    getMemoryMax,
    lastOomPath,
  )
where

import Control.Monad (forever, void, when)
import Control.Monad.Catch (Exception, MonadCatch, MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Generics.Labels ()
import Data.Generics.Product (HasType (typed))
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector, (!?))
import Data.Word (Word64)
import Database.PostgreSQL.Simple
  ( Connection,
    FromRow,
    Query,
    SqlError,
    ToRow,
    execute,
    withTransaction,
  )
import Database.PostgreSQL.Simple.Vector (query)
import Inferno.ML.Server.Types
import Inferno.VersionControl.Types (VCObjectHash)
import Lens.Micro.Platform (view)
import System.Exit (ExitCode (ExitFailure))
import System.FilePath ((</>))
import System.IO.Error
  ( isEOFError,
    isIllegalOperation,
    isResourceVanishedError,
  )
import System.Posix.Process (exitImmediately)
import UnliftIO (MonadUnliftIO (withRunInIO))
import UnliftIO.Async (cancel, waitEitherCatch, withAsync)
import UnliftIO.Concurrent (forkIO, threadDelay)
import UnliftIO.Directory (doesPathExist)
import UnliftIO.Exception
  ( SomeException,
    catch,
    catchJust,
    displayException,
    fromException,
    mask_,
    throwIO,
  )
import UnliftIO.IO
  ( Handle,
    IOMode (ReadMode),
    SeekMode (AbsoluteSeek),
    hSeek,
    withFile,
  )

throwRemoteError :: RemoteError -> RemoteM a
throwRemoteError = throwM

throwInfernoError :: (Exception e) => Id InferenceParam -> VCObjectHash -> e -> RemoteM a
throwInfernoError ipid vch =
  throwRemoteError . InfernoError ipid vch . SomeInfernoError . show

catchDb :: (MonadThrow m, MonadCatch m, MonadUnliftIO m) => m a -> m a
catchDb f = catch @_ @SqlError f $ (throwM @_ @RemoteError) . DbError . displayException

type HasPool r m =
  ( MonadUnliftIO m
  , MonadReader r m
  , MonadCatch m
  , MonadThrow m
  , HasType (Pool Connection) r
  )

queryStore :: forall b a m r. (HasPool r m, ToRow b, FromRow a) => Query -> b -> m (Vector a)
queryStore q x = catchDb . withConns $ \conn -> liftIO $ query conn q x
{-# INLINE queryStore #-}

executeStore :: forall a m r. (HasPool r m, ToRow a) => Query -> a -> m ()
executeStore q x =
  catchDb . withConns $ \conn ->
    liftIO . withTransaction conn . void $
      execute conn q x
{-# INLINE executeStore #-}

firstOrThrow :: (MonadThrow m) => RemoteError -> Vector a -> m a
firstOrThrow e = maybe (throwM e) pure . (!? 0)
{-# INLINE firstOrThrow #-}

withConns :: (HasPool r m) => (Connection -> m b) -> m b
withConns f = view typed >>= \cs -> withRunInIO $ \r -> withResource cs $ r . f
{-# INLINE withConns #-}

-- | Try to run a @RemoteM@ process while monitoring its memory usage. If
-- the value of the process @memory.current@ comes within a close threshold of
-- the server\'s @MemoryMax@ acccording to its systemd service configuration,
-- the action is cancelled and an exception is raised. The server process is
-- restarted to reclaim memory
--
-- NOTE: If the cgroup @memory.max@ information is missing or unreadable at the
-- time of server start, the provided @RemoteM@ action is run /without/ memory
-- monitoring and a warning is logged
--
-- Even without the active monitor below, if the entire server process exceeds
-- @MemoryMax@ it will be stopped and restarted. However, it\'s preferable to
-- first return the response with the exception as the response body for a more
-- meaningful error message
--
-- NOTE: This is really only meant to be used with the script evaluator, which
-- is the only server endpoint that can consume a significant amount of memory.
-- But it\'s easier\/cleaner to add this as a CPS-style wrapper without including
-- the logic directly inside the script evaluator
withMemoryMonitor :: forall a. RemoteM a -> RemoteM a
withMemoryMonitor f =
  view #memoryMax >>= \case
    -- This means that the `memory.max` cgroup information was missing or
    -- unreadable when starting the server; in that case we just perform the
    -- action without memory monitoring. Note that we still have the systemd
    -- OOM policy as a backup so the action will still be killed, but it's not
    -- as graceful (i.e. either way we can't exceed the limit)
    Nothing -> logCantMonitor "Server `memory.max` missing or unreadable" *> f
    Just memMax ->
      doesPathExist memoryCurrentPath >>= \case
        False -> logCantMonitor "Server `memory.current` missing" *> f
        True ->
          -- Open handle to `$CGROUP/memory.current` so we don't open the same
          -- file constantly. The `Handle` will be read into a `ByteString` and
          -- parsed into a `Word64`
          withFile memoryCurrentPath ReadMode $ \h ->
            withAsync (monitor h) $ \mon ->
              withAsync f $ \inner ->
                waitEitherCatch mon inner >>= \case
                  -- Here the monitor has failed, i.e. thrown an exception, so
                  -- cancel `f` and re-throw the exception
                  Left (Left e) -> do
                    cancel inner
                    -- Schedule a restart for 500ms from now, giving enough time
                    -- for warp handler to send the response with the correct body
                    -- (i.e. the synchronously rethrown exception below). Exit
                    -- code of 1 for generic non-success
                    --
                    -- NOTE: We do a restart here because even try to force
                    -- memory reclamation both Torch FFI and Haskell GC does
                    -- not seem to free enough memory. Then we are stuck in
                    -- a position where the memory monitor will fail, but we
                    -- don't get restarted by systemd. However, here we can send
                    -- the response with a clear body describing the problem and
                    -- release the memory by killing the process
                    scheduleSelfRestart 500_000 1
                    throwRemoteError $ toRemoteError e
                  -- Should never happen; monitor runs in infinite loop until it
                  -- throws an exception
                  Left (Right _) ->
                    throwRemoteError $
                      OtherRemoteError "Memory monitor terminated abnormally"
                  -- Here `f` has failed, so cancel the monitor thread and
                  -- re-throw. This would be a normal failure from the
                  -- script evaluator, not due to memory usage
                  Right (Left e) -> do
                    cancel mon
                    throwRemoteError $ toRemoteError e
                  -- `f` has succeeded
                  Right (Right x) -> x <$ cancel mon
      where
        -- Thread to monitor memory usage by reading `$CGROUP/memory.current`
        -- and comparing it to `memory.max` (which is stored in the `Env`).
        -- Once the current memory usage gets close enough to the maximum,
        -- `MemoryLimitExceeded` is raised; this propagates to the main thread
        -- and the action running concurrently
        monitor ::
          -- Open (read-only) file handle to `$CGROUP/memory.current`, which
          -- we will try to read into bytes. Note that this handle is obtained
          -- from `withFile` above, which will close it upon completion or
          -- exception
          Handle ->
          RemoteM a
        monitor h = forever $ do
          -- Seek back to beginning of handle to avoid EOF issues when reading
          -- from it into a bytestring and parsing out the `Word64`
          hSeek h AbsoluteSeek 0
          -- Try to read the current memory usage in bytes from the handle
          readMemoryCurrent >>= \case
            -- In this case, we will just log a warning; we don't want to kill
            -- the thread because we are going to try again shortly anyway
            Nothing ->
              logWarn . CantMonitorMemory $ "Server `memory.current` unreadable"
            Just current ->
              -- If the current >= limit, then we have reached a threshold where
              -- the OOM killer is likely to be invoked. This throws an exception,
              -- which propagates to the calling thread, which also includes the
              -- `RemoteM` effect in its child scope, i.e. also killing it
              when (current >= limit) . throwRemoteError $
                MemoryLimitExceeded current
          -- Wait 200ms
          threadDelay 200_000
          where
            -- Try to read the `Word64` (memory in bytes) from the open file handle
            readMemoryCurrent :: RemoteM (Maybe Word64)
            readMemoryCurrent =
              maybe Nothing (fmap fst . ByteString.Char8.readWord64)
                <$> mreadHandle
              where
                -- Try reading the open handle into a `ByteString`, with
                -- consideration for possible `IOError`s that might arise
                mreadHandle :: RemoteM (Maybe ByteString)
                mreadHandle =
                  liftIO
                    . catchJust select (fmap Just (ByteString.Char8.hGetLine h))
                    . const
                    $ pure Nothing
                  where
                    select :: IOError -> Maybe ()
                    select e
                      | isEOFError e = Just ()
                      | isIllegalOperation e = Just ()
                      | isResourceVanishedError e = Just ()
                      | otherwise = Nothing

        toRemoteError :: SomeException -> RemoteError
        toRemoteError e =
          fromMaybe ((OtherRemoteError . Text.pack . displayException) e) $
            fromException e

        -- This is the limit for memory usage in bytes, i.e. 98% of `MemoryMax`.
        -- If we hit this threshold is is likely we will exceed the maximum and
        -- the thread is killed
        limit :: Word64
        limit = round @Float $ realToFrac memMax * 0.98

        memoryCurrentPath :: FilePath
        memoryCurrentPath = cgroupPath </> "memory.current"
  where
    logCantMonitor :: Text -> RemoteM ()
    logCantMonitor = logWarn . CantMonitorMemory

-- | Get the maximum memory in bytes that the server process can use according
-- to its systemd policy. Note:
--   * if the cgroup @memory.max@ does not exist, @Nothing@ is returned
--   * unparseable contents such as @infinity@, @max@ etc... also return @Nothing@
--
-- This is a constant value and depends on the value of the @MemoryMax@ defined
-- for the server\'s systemd service
getMemoryMax :: (MonadIO m) => m (Maybe Word64)
getMemoryMax = getCgroupMemInfo "memory.max"

-- | Try to parse memory in bytes from a file under the server cgroup directory
getCgroupMemInfo ::
  (MonadIO m) =>
  -- | Path to relevant file in cgroup directory, e.g. @memory.max@, @memory.current@
  FilePath ->
  m (Maybe Word64)
getCgroupMemInfo fp =
  doesPathExist path >>= \case
    False -> pure Nothing
    True ->
      fmap fst . ByteString.Char8.readWord64
        <$> liftIO (ByteString.Char8.readFile path)
  where
    path :: FilePath
    path = cgroupPath </> fp

-- | Path to the cgroup information for the @inferno-ml-server@ systemd service.
-- This is needed for e.g. getting information on current memory usage. Note
-- that this path is readable for the @inferno@ user, including its contents
cgroupPath :: FilePath
cgroupPath = "/sys/fs/cgroup/system.slice/inferno-ml-server.service"

-- | This path is written to as part of the @inferno-ml-server@ @ExecStopPost@
-- if an OOM kill event is the @SERVICE_RESULT@. This is read at startup to
-- log a warning if we just restarted the server due to an OOM kill
lastOomPath :: FilePath
lastOomPath = infernoMlStateDirectory </> "last-oom"

-- | Force a restart of the entire server process internally. This forks a
-- thread that waits for the given amount of microseconds before exiting
-- immediately.
--
-- NOTE: Exit code MUST be non-zero for systemd to restart service
scheduleSelfRestart :: (MonadUnliftIO m) => Int -> Int -> m ()
scheduleSelfRestart micros code
  | code <= 0 = throwIO $ userError "`scheduleRestart`: exit code must be non-zero"
  | otherwise =
      -- NOTE: `mask_` here because we DON'T want to be killed by any async
      -- exceptions. This is meant to kill the process without any interruption
      mask_ . void . forkIO $ do
        -- Delaying gives Warp the necessary amount of time for the response to
        -- be sent
        threadDelay micros
        liftIO . exitImmediately $ ExitFailure code
