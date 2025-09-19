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
    getMemoryMax,
    getMemoryCurrent,
    lastOomPath,
  )
where

import Control.Monad (void)
import Control.Monad.Catch (Exception, MonadCatch, MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Generics.Labels ()
import Data.Generics.Product (HasType (typed))
import Data.Pool (Pool, withResource)
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
import UnliftIO.Directory (doesPathExist)
import UnliftIO.Exception (catch, displayException)

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

-- | Get the maximum memory in bytes that the server process can use according
-- to its systemd policy. Note:
--   * if the cgroup @memory.max@ does not exist, @Nothing@ is returned
--   * unparseable contents such as @infinity@, etc... will result in @Nothing@
--
-- This is a constant value
getMemoryMax :: (MonadIO m) => m (Maybe Word64)
getMemoryMax = getCgroupMemInfo "memory.max"

-- | Get the server\'s current memory usage in bytes using its cgroup information.
-- Note:
--   * if the cgroup @memory.current@ does not exist, @Nothing@ is returned
--   * unparseable contents will result in @Nothing@
getMemoryCurrent :: (MonadIO m) => m (Maybe Word64)
getMemoryCurrent = getCgroupMemInfo "memory.current"

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
