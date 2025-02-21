{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Utils
  ( throwInfernoError,
    firstOrThrow,
    queryStore,
    executeStore,
    withConns,
  )
where

import Control.Monad (void)
import Control.Monad.Catch (Exception, MonadThrow (throwM))
import Control.Monad.IO.Class (liftIO)
import Data.Generics.Labels ()
import Data.Pool (withResource)
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

throwInfernoError :: forall e a. (Exception e) => Either e a -> RemoteM a
throwInfernoError = either (throwM . InfernoError . SomeInfernoError) pure

queryStore :: (ToRow b, FromRow a) => Query -> b -> RemoteM (Vector a)
queryStore q x = withConns $ \conn -> liftIO $ query conn q x

executeStore :: (ToRow a) => Query -> a -> RemoteM ()
executeStore q x =
  withConns $ \conn ->
    liftIO . withTransaction conn . void $
      execute conn q x

firstOrThrow :: (MonadThrow m, Exception e) => e -> Vector a -> m a
firstOrThrow e = maybe (throwM e) pure . (!? 0)

withConns :: (Connection -> RemoteM b) -> RemoteM b
withConns f = view #store >>= \cs -> withRunInIO $ \r -> withResource cs $ r . f
