{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Inference.Model
  ( getModelsAndVersions,
    getModelVersionSizeAndContents,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Vector (Vector)
import Database.PostgreSQL.Simple
  ( In (In),
    Only (Only, fromOnly),
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
    withConns,
  )
import UnliftIO (MonadUnliftIO (withRunInIO))
import UnliftIO.Exception (bracket)

-- | Get an array of model versions along with the parent model of each; note
-- that this does not retrieve the model version contents -- each version only
-- contains the 'Oid' of the large object
getModelsAndVersions ::
  Vector (Id ModelVersion) -> RemoteM (Vector (Model, ModelVersion))
getModelsAndVersions =
  fmap (fmap joinToTuple)
    . queryStore q
    . Only
    . In
    . toList
  where
    q :: Query
    q =
      [sql|
        SELECT M.*, V.*
        FROM mversions V
          INNER JOIN models M ON V.model = M.id
        WHERE V.id IN ?
          AND V.terminated IS NULL
          AND M.terminated IS NULL
      |]

-- | Get the actual serialized bytes of the model, which is stored in the Postgres
-- large object table (and must be explicitly imported using 'loImport'), along
-- with the number of bytes
getModelVersionSizeAndContents :: Oid -> RemoteM (Integer, ByteString)
getModelVersionSizeAndContents m =
  withConns $ \conn -> withRunInIO $ \r ->
    withTransaction conn . r $ do
      size <- getModelVersionSize m
      bs <-
        liftIO
          . bracket (loOpen conn m ReadMode) (loClose conn)
          . flip (loRead conn)
          $ fromIntegral size
      pure (size, bs)

-- | Get the size of the model contents themselves (byte count of large object).
-- It is better to do this via Postgres rather than using @ByteString.length@
-- on the returned bytes
getModelVersionSize :: Oid -> RemoteM Integer
getModelVersionSize oid =
  fmap fromOnly $
    firstOrThrow (OtherRemoteError "Could not get model size")
      =<< queryStore q (Only oid)
  where
    q :: Query
    q = [sql| SELECT length(lo_get(?)) |]
