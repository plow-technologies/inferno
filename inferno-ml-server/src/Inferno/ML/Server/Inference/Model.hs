{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Inference.Model
  ( getModelsAndVersions,
    getModelVersionContents,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Vector (Vector)
import Database.PostgreSQL.Simple
  ( In (In),
    Only (Only),
    Query,
    withTransaction,
  )
import Database.PostgreSQL.Simple.LargeObjects
  ( IOMode (ReadMode),
    loClose,
    loOpen,
    loRead,
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Inferno.ML.Server.Types
import Inferno.ML.Server.Utils
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
getModelVersionContents :: ModelVersion -> RemoteM ByteString
getModelVersionContents mversion =
  withConns $ \conn -> withRunInIO $ \r ->
    withTransaction conn . r $ do
      liftIO
        . bracket (loOpen conn mversion.contents ReadMode) (loClose conn)
        . flip (loRead conn)
        $ fromIntegral mversion.size
