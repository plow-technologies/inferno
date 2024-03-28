{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Inference.Model
  ( getModel,
    getModelVersion,
    getModelSizeAndContents,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.Generics.Wrapped (wrappedTo)
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
import Inferno.ML.Server.Types
import Inferno.ML.Server.Utils
  ( firstOrThrow,
    queryStore,
  )
import Lens.Micro.Platform
import UnliftIO (MonadUnliftIO (withRunInIO))
import UnliftIO.Exception (bracket)

-- | Get the model row itself. This is to access things like the name,
-- permissions, etc... that are not contained in the model version table
-- (see 'getModelVersion')
getModel :: Id Model -> RemoteM Model
getModel mid =
  firstOrThrow (NoSuchModel (wrappedTo mid))
    =<< queryStore q (Only mid)
  where
    q :: Query
    q = [sql| SELECT * FROM models WHERE id = ? |]

-- Get a row from the model versions table, which contains the actual contents,
-- description, etc... The foreign key of the version row can be used to get
-- the invariant model metadata (see 'getModel')
--
-- This does not include the actual contents of the model, which need to be
-- fetched separately using 'loImport'
getModelVersion :: Id ModelVersion -> RemoteM ModelVersion
getModelVersion mid =
  firstOrThrow (NoSuchModel (wrappedTo mid))
    =<< queryStore q (Only mid)
  where
    q :: Query
    q = [sql| SELECT * FROM mversions WHERE id = ? |]

-- | Get the actual serialized bytes of the model, which is stored in the Postgres
-- large object table (and must be explicitly imported using 'loImport'), along
-- with the number of bytes
getModelSizeAndContents :: Oid -> RemoteM (Integer, ByteString)
getModelSizeAndContents m =
  view #store >>= \conn -> withRunInIO $ \r ->
    withTransaction conn . r $ do
      size <- getModelSize m
      bs <-
        liftIO
          . bracket (loOpen conn m ReadMode) (loClose conn)
          . flip (loRead conn)
          $ fromIntegral size
      pure (size, bs)

-- | Get the size of the model contents themselves (byte count of large object).
-- It is better to do this via Postgres rather than using @ByteString.length@
-- on the returned bytes
getModelSize :: Oid -> RemoteM Integer
getModelSize oid =
  fmap fromOnly $
    firstOrThrow (OtherRemoteError "Could not get model size")
      =<< queryStore q (Only oid)
  where
    q :: Query
    q = [sql| SELECT length(lo_get(?)) |]
