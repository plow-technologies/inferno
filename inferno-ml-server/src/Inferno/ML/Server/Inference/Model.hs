{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Inference.Model
  ( getModelsAndVersions,
    getTorchScriptModelContents,
    cacheAndLoadModel,
    mkModelPath,
  )
where

import Control.Arrow ((&&&))
import Control.Monad ((<=<))
import Control.Monad.Extra (loopM, unlessM, whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.ListM (sortByM)
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import Data.Word (Word64)
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
import System.DiskSpace (DiskUsage, getDiskUsage)
import qualified System.DiskSpace
import System.FilePath ((<.>), (</>))
import Torch (LoadMode (WithoutRequiredGrad), ScriptModule)
import qualified Torch.Script
import UnliftIO (MonadUnliftIO (withRunInIO))
import UnliftIO.Directory
  ( doesPathExist,
    getAccessTime,
    listDirectory,
    removeFile,
  )
import UnliftIO.Exception (bracket, catchIO, displayException)
import UnliftIO.IO.File (writeBinaryFileDurableAtomic)

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

-- | Cache and load a model by its UUID. If already cached, loads directly.
-- Otherwise fetches from DB, checks disk space (evicting LRU models if needed),
-- writes to cache, then loads
cacheAndLoadModel :: UUID -> RemoteM ScriptModule
cacheAndLoadModel uuid = do
  unlessM (doesPathExist path) $ do
    logInfo . CopyingModel $ Id uuid
    copyModelVersion =<< getModelVersion
  -- NOTE: Any exceptions from `loadScript` are caught in the `loadModel`
  -- primitive and converted to `RuntimeError`s there
  liftIO $ Torch.Script.loadScript WithoutRequiredGrad path
  where
    path :: FilePath
    path = modelCachePath </> mkModelPath uuid

    copyModelVersion :: ModelVersion -> RemoteM ()
    copyModelVersion mversion = do
      checkDiskSpace mversion.size
      writeBinaryFileDurableAtomic path
        =<< getTorchScriptModelContents mversion

    getModelVersion :: RemoteM ModelVersion
    getModelVersion =
      firstOrThrow (NoSuchModel (Right (Id uuid)))
        =<< queryStore q (Only uuid)
      where
        q :: Query
        q =
          [sql|
            SELECT V.*
            FROM mversions V
              INNER JOIN models M ON V.model = M.id
            WHERE V.id = ?
              AND V.terminated IS NULL
              AND M.terminated IS NULL
          |]

    -- Check if adding a model would exceed 90% disk usage; if so, evict LRU models
    checkDiskSpace :: Word64 -> RemoteM ()
    checkDiskSpace modelSize =
      flip whenM evictOldModels . wouldExceedThreshold $ fromIntegral modelSize

    evictOldModels :: RemoteM ()
    evictOldModels = loopM doEvict =<< modelsByAccessTime modelCachePath

    doEvict :: [FilePath] -> RemoteM (Either [FilePath] ())
    doEvict = \case
      [] -> pure $ Right ()
      m : ms ->
        wouldExceedThreshold 0 >>= \case
          False -> pure $ Right ()
          True -> Left ms <$ tryRemoveFile (modelCachePath </> m)
      where
        tryRemoveFile :: FilePath -> RemoteM ()
        tryRemoveFile fp =
          catchIO (removeFile fp) $
            logWarn . OtherWarn . Text.pack . displayException

    -- Check if adding `extra` bytes would exceed 90% disk usage
    wouldExceedThreshold :: Integer -> RemoteM Bool
    wouldExceedThreshold extra =
      uncurry (>=) . (allUsage &&& threshold) <$> liftIO (getDiskUsage "/")
      where
        allUsage :: DiskUsage -> Integer
        allUsage usage =
          extra + (fromIntegral usage.diskTotal - fromIntegral usage.diskAvail)

        threshold :: DiskUsage -> Integer
        threshold usage = (fromIntegral usage.diskTotal * 9) `div` 10

-- | Get models sorted by access time (least recently used first)
modelsByAccessTime :: (MonadIO m) => FilePath -> m [FilePath]
modelsByAccessTime = sortByM cmpAccessTime <=< listDirectory
  where
    cmpAccessTime :: (MonadIO m) => FilePath -> FilePath -> m Ordering
    cmpAccessTime f1 f2 = compare <$> getAccessTime f1 <*> getAccessTime f2

-- | Generate a filepath from a model version UUID
mkModelPath :: UUID -> FilePath
mkModelPath = (<.> "ts" <.> "pt") . UUID.toString
