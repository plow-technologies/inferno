{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Utils
  ( throwInfernoError,
    firstOrThrow,
    queryStore,
    bridgeCache,
  )
where

import Control.Monad.Catch (Exception, MonadThrow (throwM))
import Control.Monad.IO.Class (liftIO)
import Data.Generics.Labels ()
import Data.Vector (Vector, (!?))
import Database.PostgreSQL.Simple (FromRow, Query, ToRow)
import Database.PostgreSQL.Simple.Vector (query)
import Inferno.ML.Server.Types
import Lens.Micro.Platform (view)

throwInfernoError :: forall e a. Exception e => Either e a -> RemoteM a
throwInfernoError = either (throwM . InfernoError . SomeInfernoError) pure

queryStore :: (ToRow b, FromRow a) => Query -> b -> RemoteM (Vector a)
queryStore q x = view #store >>= \conn -> liftIO $ query conn q x

firstOrThrow :: (MonadThrow m, Exception e) => e -> Vector a -> m a
firstOrThrow e = maybe (throwM e) pure . (!? 0)

-- | Pathe to the cached bridge info; the server will save the info when the
-- bridge is registered
bridgeCache :: FilePath
bridgeCache = "/home/inferno/.cache/bridge/info.json"
