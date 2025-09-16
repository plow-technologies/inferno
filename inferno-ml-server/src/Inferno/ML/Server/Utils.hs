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
    lastOomPath,
  )
where

import Control.Monad (void)
import Control.Monad.Catch (Exception, MonadCatch, MonadThrow (throwM))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Generics.Labels ()
import Data.Generics.Product (HasType (typed))
import Data.Pool (Pool, withResource)
import Data.Vector (Vector, (!?))
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
import UnliftIO (MonadUnliftIO (withRunInIO))
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

-- | This path is written to as part of the @inferno-ml-server@ @ExecStopPost@
-- if an OOM kill event is the @SERVICE_RESULT@. This is read at startup to
-- log a warning if we just restarted the server due to an OOM kill
lastOomPath :: FilePath
lastOomPath = "/var/lib/inferno-ml-server/last-oom"
