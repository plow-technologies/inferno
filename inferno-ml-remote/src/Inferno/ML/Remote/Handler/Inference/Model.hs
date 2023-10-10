{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Remote.Handler.Inference.Model
  ( getModel,
    getModelContents,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (asks)
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple
  ( Only (Only, fromOnly),
    Query,
    withTransaction,
  )
import Database.PostgreSQL.Simple.LargeObjects
  ( IOMode (ReadMode),
    Oid,
    loClose,
    loOpen,
    loRead,
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Inferno.ML.Remote.Handler.Utils
  ( firstOrThrow,
    queryStore,
  )
import Inferno.ML.Remote.Types
  ( Model,
    RemoteError (NoSuchModel, OtherError),
    RemoteM,
    RequestedModel,
  )
import Lens.Micro.Platform (view, (^.))
import UnliftIO (MonadUnliftIO (withRunInIO))
import UnliftIO.Exception (bracket)

getModel :: RequestedModel -> RemoteM Model
getModel rm =
  firstOrThrow (NoSuchModel (view #name rm))
    =<< queryStore q (rm ^. #name, rm ^. #version)
  where
    -- NOTE
    -- In the future, users will be able to add their own models. This is not
    -- going to be supported in the intial version, hence the `NULL` filter on
    -- `users`
    q :: Query
    q =
      [sql|
        SELECT * FROM models
        WHERE name = ?
        AND version = ?
        AND "user" IS NULL
      |]

getModelContents :: Oid -> RemoteM (Integer, ByteString)
getModelContents m =
  asks (view #store) >>= \conn -> withRunInIO $ \r ->
    withTransaction conn . r $ do
      size <- getModelSize m
      bs <- liftIO . bracket (loOpen conn m ReadMode) (loClose conn) $
        \fd -> loRead conn fd $ fromIntegral size
      pure (size, bs)

getModelSize :: Oid -> RemoteM Integer
getModelSize oid =
  fmap fromOnly $
    firstOrThrow (OtherError "Could not get model size")
      =<< queryStore q (Only oid)
  where
    q :: Query
    q = [sql| SELECT length(lo_get(?)) |]
