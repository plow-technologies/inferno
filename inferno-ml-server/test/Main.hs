{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- These test items do not run a full `inferno-ml-server` instance and only
-- check that a limited subset of server operations work as intended (e.g. model
-- fetching and caching). For full server tests, see `tests/server.nix`.
-- Note that the `mnist` model and its first version must be saved to the DB
-- before running this test

module Main (main) where

import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (eitherDecodeFileStrict)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (toList, traverse_)
import Data.Generics.Wrapped (wrappedTo)
import qualified Data.Map.Strict as Map
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word8)
import Inferno.ML.Server (runInEnv)
import Inferno.ML.Server.Inference (getAndCacheModels)
import Inferno.ML.Server.Inference.Model
  ( getModelsAndVersions,
    getTorchScriptModelContents,
  )
import Inferno.ML.Server.Types
import Lens.Micro.Platform
import Plow.Logging.Message (LogLevel (LevelWarn))
import System.FilePath ((<.>))
import Test.Hspec (Spec)
import qualified Test.Hspec as Hspec
import UnliftIO (throwString)
import UnliftIO.Directory
  ( getCurrentDirectory,
    getModificationTime,
    listDirectory,
    removeFile,
    withCurrentDirectory,
  )
import UnliftIO.Environment (getArgs, withArgs)

main :: IO ()
main =
  getArgs >>= \case
    cfg : args ->
      (`runInEnv` runTests args)
        =<< either throwString (pure . (`Config` perServerConfig))
        =<< eitherDecodeFileStrict @GlobalConfig cfg
    _ -> throwString "Missing path to configuration file"
  where
    perServerConfig :: PerServerConfig
    perServerConfig = PerServerConfig "dummy" LevelWarn
    runTests :: [String] -> Env -> IO ()
    runTests args env = withArgs args . Hspec.hspec $ do
      mkDbSpec env
      mkCacheSpec env

mkCacheSpec :: Env -> Spec
mkCacheSpec env = Hspec.before_ clearCache . Hspec.describe "Model cache" $ do
  Hspec.it "caches a model" . cdCache $ do
    cacheModel
    dir <- listDirectory env.config.global.cache.path
    dir `Hspec.shouldMatchList` [mnistV1Path]
    contents <- ByteString.readFile mnistV1Path
    ByteString.length contents `Hspec.shouldBe` mnistV1Size
    getZipMagic contents `Hspec.shouldBe` zipMagic

  Hspec.it "doesn't re-cache" . cdCache $ do
    atime1 <- cacheModel *> getModificationTime mnistV1Path
    atime2 <- cacheModel *> getModificationTime mnistV1Path
    atime2 `Hspec.shouldBe` atime1
  where
    cacheModel :: IO ()
    cacheModel =
      void . flip runReaderT env $
        (`getAndCacheModels` modelsWithIdents)
          =<< view (#config . #global . #cache)

    clearCache :: IO ()
    clearCache =
      cdCache $
        traverse_ removeFile
          =<< listDirectory
          =<< getCurrentDirectory

    cdCache :: IO a -> IO a
    cdCache = withCurrentDirectory env.config.global.cache.path

modelsWithIdents :: Models (Id ModelVersion)
modelsWithIdents = Map.singleton "dummy" mnistV1

mkDbSpec :: Env -> Spec
mkDbSpec env = Hspec.describe "Database" $ do
  Hspec.it "gets a model" $ do
    runReaderT (getModelsAndVersions modelVersions) env >>= \case
      v
        | Just (model, mversion) <- v ^? _head -> do
            model.name `Hspec.shouldBe` "mnist"
            showVersion mversion.version `Hspec.shouldBe` "v1"
        | otherwise -> Hspec.expectationFailure "No models were retrieved"

  Hspec.it "gets model size and contents" $ do
    getWithContents env >>= \case
      (mversion, contents) -> do
        -- This size is computed using PG functions
        mversion.size `Hspec.shouldBe` fromIntegral mnistV1Size
        -- This is the magic number for a ZIP
        getZipMagic contents `Hspec.shouldBe` zipMagic
        -- The size from PG and the size of the bytestring should be
        -- the same
        ByteString.length contents `Hspec.shouldBe` fromIntegral mversion.size

getWithContents :: Env -> IO (ModelVersion, ByteString)
getWithContents env = flip runReaderT env $ do
  (getModelsAndVersions modelVersions >>=) . (. fmap snd . toList) $ \case
    [] -> throwString "No model was retrieved"
    v : _ -> (v,) <$> getTorchScriptModelContents v

mnistV1 :: Id ModelVersion
mnistV1 = Id $ UUID.fromWords 6 0 0 0

mnistV1Path :: FilePath
mnistV1Path = UUID.toString (wrappedTo mnistV1) <.> "ts" <.> "pt"

modelVersions :: Vector (Id ModelVersion)
modelVersions = Vector.singleton mnistV1

mnistV1Size :: Int
mnistV1Size = 4808991

-- Magic header for a zip file (the underlying format of a `.ts.pt`)
zipMagic :: [Word8]
zipMagic = [80, 75, 3, 4]

getZipMagic :: ByteString -> [Word8]
getZipMagic = take 4 . ByteString.unpack
