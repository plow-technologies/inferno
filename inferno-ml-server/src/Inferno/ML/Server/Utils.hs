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
  )
where

import Control.Monad (void)
import Control.Monad.Catch (Exception, MonadThrow (throwM))
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
    ToRow,
    execute,
    withTransaction,
  )
import Database.PostgreSQL.Simple.Vector (query)
import Inferno.ML.Server.Types
import Lens.Micro.Platform (view)
import UnliftIO (MonadUnliftIO (withRunInIO))

throwRemoteError :: RemoteError -> RemoteM a
throwRemoteError = throwM

throwInfernoError :: (Exception e) => Id InferenceParam -> e -> RemoteM a
throwInfernoError ipid = throwRemoteError . InfernoError ipid . SomeInfernoError . show

type HasPool r m = (MonadUnliftIO m, MonadReader r m, HasType (Pool Connection) r)

queryStore :: forall b a m r. (HasPool r m, ToRow b, FromRow a) => Query -> b -> m (Vector a)
queryStore q x = withConns $ \conn -> liftIO $ query conn q x
{-# INLINE queryStore #-}

executeStore :: forall a m r. (HasPool r m, ToRow a) => Query -> a -> m ()
executeStore q x =
  withConns $ \conn ->
    liftIO . withTransaction conn . void $
      execute conn q x
{-# INLINE executeStore #-}

firstOrThrow :: (MonadThrow m) => RemoteError -> Vector a -> m a
firstOrThrow e = maybe (throwM e) pure . (!? 0)
{-# INLINE firstOrThrow #-}

withConns :: (HasPool r m) => (Connection -> m b) -> m b
withConns f = view typed >>= \cs -> withRunInIO $ \r -> withResource cs $ r . f
{-# INLINE withConns #-}
