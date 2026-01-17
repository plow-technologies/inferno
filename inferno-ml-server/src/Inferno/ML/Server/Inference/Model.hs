{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Inference.Model
  ( getModelsAndVersions,
    getTorchScriptModelContents,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import qualified Data.Text as Text
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

-- | Get the actual serialized bytes of a TorchScript model, which is stored in
-- the Postgres large object table (and must be explicitly imported using 'loImport'),
-- along with the number of bytes
--
-- TODO/NOTE This will ONLY be invoked in the future when a model has a @TorchScript@
-- @ModelConfig@. At the moment, it is invoked for all models. This is the least
-- intrusive change (i.e. throwing an exception if a @Bedrock@ model sneaks in).
-- We have no path to creating @Bedrock@ at the moment (except via direct database
-- manipulation), so this is reasonable for now
getTorchScriptModelContents :: ModelVersion -> RemoteM ByteString
getTorchScriptModelContents mversion =
  case mversion.contents of
    Bedrock _ ->
      throwRemoteError . OtherRemoteError $
        Text.unwords
          [ "Model"
          , tshow mversion.id
          , "is a Bedrock model; cannot be used as a TorchScript model"
          ]
    TorchScript oid ->
      withConns $ \conn -> withRunInIO $ \r ->
        withTransaction conn . r $ do
          liftIO
            . bracket (loOpen conn oid ReadMode) (loClose conn)
            . flip (loRead conn)
            $ fromIntegral mversion.size
