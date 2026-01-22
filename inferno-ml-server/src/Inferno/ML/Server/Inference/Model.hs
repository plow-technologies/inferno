{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Inference.Model
  ( loadModel,
  )
where

import Control.Arrow ((&&&))
import Control.Monad.Extra (loopM, unlessM, whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.ListM (sortByM)
import Data.ByteString (ByteString)
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Word (Word64)
import Database.PostgreSQL.Simple
  ( Only (Only),
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

-- | Load either a @TorchScript@ model, which will also be cached in the
-- model directory of the server, or a @Bedrock@ model, in which case the
-- model configuration is simply held in memory
loadModel :: UUID -> RemoteM (ModelConfig ScriptModule)
loadModel uuid = getModelVersion uuid >>= \mversion ->
  case mversion.contents of
    TorchScript oid ->
        fmap TorchScript . cacheTorchScriptModel $
          TorchScriptModel uuid mversion.size oid
    -- For `Bedrock`, we can just use the configuration straight from the DB
    Bedrock cfg -> pure $ Bedrock cfg

-- | Cache and load a model by its UUID. If already cached, loads directly.
-- Otherwise fetches from DB, checks disk space (evicting LRU models if needed),
-- writes to cache, then loads into a Torch @ScriptModule@
--
-- NOTE: This only applies to @TorchScript@ models, which have actual contents
-- (the blob of the @.ts.pt@); @Bedrock@ models are loaded differently
cacheTorchScriptModel :: TorchScriptModel -> RemoteM ScriptModule
cacheTorchScriptModel tsm = do
  unlessM (doesPathExist path) $ do
    logInfo . CopyingModel $ Id tsm.id
    checkDiskSpace tsm.size
    writeBinaryFileDurableAtomic path =<< getTorchScriptModelContents
  -- NOTE: Any exceptions from `loadScript` are caught in the `loadModel`
  -- primitive and converted to `RuntimeError`s there
  liftIO $ Torch.Script.loadScript WithoutRequiredGrad path
  where
    path :: FilePath
    path = modelCachePath </> UUID.toString tsm.id <.> "ts" <.> "pt"

    -- Get the actual serialized bytes of a TorchScript model, which is stored in
    -- the Postgres large object table (and must be explicitly imported using 'loImport'),
    -- along with the number of bytes
    getTorchScriptModelContents :: RemoteM ByteString
    getTorchScriptModelContents =
      withConns $ \conn -> withRunInIO $ \r ->
        withTransaction conn . r $
          liftIO
            . bracket (loOpen conn tsm.oid ReadMode) (loClose conn)
            . flip (loRead conn)
            $ fromIntegral tsm.size

    -- Check if adding a model would exceed 90% disk usage; if so, evict LRU models
    checkDiskSpace :: Word64 -> RemoteM ()
    checkDiskSpace modelSize =
      flip whenM evictOldModels . wouldExceedThreshold $ fromIntegral modelSize

    evictOldModels :: RemoteM ()
    evictOldModels = loopM doEvict =<< modelsByAccessTime
      where
        -- Get cached `TorchScript` models sorted by access time (LRU first)
        modelsByAccessTime :: RemoteM [FilePath]
        modelsByAccessTime =
          sortByM cmpAccessTime
            =<< listDirectory modelCachePath
          where
            cmpAccessTime :: FilePath -> FilePath -> RemoteM Ordering
            cmpAccessTime f1 f2 =
              compare <$> getAccessTime f1 <*> getAccessTime f2

    doEvict :: [FilePath] -> RemoteM (Either [FilePath] ())
    doEvict = \case
      -- Either the dir started empty or we've deleted all remaining old cached
      -- model versions in this case
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

getModelVersion :: UUID -> RemoteM ModelVersion
getModelVersion uuid =
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

-- Local helper for the contents of a `TorchScript` model; this is after we have
-- retrieved the `ModelVersion` from the database; we don't want to keep
-- pattern-matching on the `contents` type, so so this bundles the needed
-- information for loading and caching the `TorchScript` model
data TorchScriptModel = TorchScriptModel
  { id :: UUID
  , size :: Word64
  , oid :: Oid
  }
