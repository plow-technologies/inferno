{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Control.Monad (forever, void, (<=<))
import Control.Monad.Catch (Exception, MonadCatch, MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Generics.Labels ()
import Data.Generics.Product (HasType (typed))
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, withResource)
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
import System.FilePath ((</>))
import UnliftIO (MonadUnliftIO (withRunInIO))
import UnliftIO.Async (cancel, waitCatch, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (doesPathExist)
import UnliftIO.Exception
  ( SomeException,
    catch,
    displayException,
    fromException,
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
-- the action is cancelled and an exception is raised
--
-- NOTE: If the cgroup @memory.max@ information is missing or unreadable at the
-- time of server start, the provided @RemoteM@ action is run /without/ memory
-- monitoring and a warning is logged
withMemoryMonitor :: RemoteM () -> RemoteM ()
withMemoryMonitor f =
  view #memoryMax >>= \case
    -- This means that the `memory.max` cgroup information was missing or
    -- unreadable when starting the server; in that case we just perform the
    -- action without memory monitoring
    Nothing ->
      (logWarn . CantMonitorMemory) "Server `memory.max` missing or unreadable"
        *> f
    Just memMax ->
      doesPathExist memoryCurrentPath >>= \case
        False ->
          (logWarn . CantMonitorMemory) "Server `memory.current` missing"
            *> f
        True ->
          -- Open handle to `$CGROUP/memory.current` so we don't open the same
          -- file constantly. The `Handle` will be read into a `ByteString` and
          -- parsed into a `Word64`
          withFile memoryCurrentPath ReadMode $ \h ->
            withAsync (monitor h) $ \mon ->
              withAsync f $
                ((cancel mon *>) . either (throwM . toRemoteError) pure)
                  <=< waitCatch
      where
        monitor :: Handle -> RemoteM ()
        monitor h = forever $ do
          -- Seek back to beginning of handle to avoid EOF issues when reading
          -- from it into a bytestring and parsing out the `Word64`
          hSeek h AbsoluteSeek 0
          -- Try to read the current memory usage in bytes from the handle
          (liftIO (ByteString.Char8.hGetLine h) >>=)
            . (. ByteString.Char8.readWord64)
            $ \case
              -- In this case, we will just log a warning; we don't want to kill
              -- the thread because we are going to try again shortly anyway
              Nothing ->
                logWarn . CantMonitorMemory $ "Server `memory.current` unreadable"
              -- FIXME Add cancellation logic here
              Just (current, _) ->
                logInfo $ OtherInfo $ "Current memory: " <> tshow current
          -- Wait 200ms
          threadDelay 200000

        toRemoteError :: SomeException -> RemoteError
        toRemoteError e =
          fromMaybe ((OtherRemoteError . Text.pack . displayException) e) $
            fromException e

        -- FIXME Multiply by percentage of max to get limit
        _limit :: Word64
        _limit = undefined memMax

        memoryCurrentPath :: FilePath
        memoryCurrentPath = cgroupPath </> "memory.current"

-- | Get the maximum memory in bytes that the server process can use according
-- to its systemd policy. Note:
--   * if the cgroup @memory.max@ does not exist, @Nothing@ is returned
--   * unparseable contents such as @infinity@, etc... will result in @Nothing@
--
-- This is a constant value and depends on the value of the @MemoryMax@ defined
-- for the server\'s systemd service
getMemoryMax :: (MonadIO m) => m (Maybe Word64)
getMemoryMax = getCgroupMemInfo "memory.max"

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
lastOomPath = "/var/lib/inferno-ml-server/last-oom"
