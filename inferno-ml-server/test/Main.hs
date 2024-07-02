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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word8)
import Inferno.ML.Server (runInEnv)
import Inferno.ML.Server.Inference
  ( getAndCacheModels,
    linkVersionedModel,
  )
import Inferno.ML.Server.Inference.Model
  ( getModelVersionSizeAndContents,
    getModelsAndVersions,
  )
import Inferno.ML.Server.Types
  ( Config,
    Env,
    Id (Id),
    ModelVersion,
    showVersion,
  )
import Inferno.Types.Syntax (Ident)
import Lens.Micro.Platform
import Plow.Logging.Message (LogLevel (LevelWarn))
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
        -- Silence logs
        =<< either throwString (pure . set #logLevel LevelWarn)
        =<< eitherDecodeFileStrict @Config cfg
    _ -> throwString "Missing path to configuration file"
  where
    runTests :: [String] -> Env -> IO ()
    runTests args env = withArgs args . Hspec.hspec $ do
      mkDbSpec env
      mkCacheSpec env

mkCacheSpec :: Env -> Spec
mkCacheSpec env = Hspec.before_ clearCache . Hspec.describe "Model cache" $ do
  Hspec.it "caches a model" . cdCache $ do
    cacheModel
    dir <- env ^. #config . #cache . #path & listDirectory
    dir `Hspec.shouldMatchList` ["mnist.v1", "mnist.ts.pt"]
    contents <- ByteString.readFile "mnist.ts.pt"
    ByteString.length contents `Hspec.shouldBe` mnistV1Size
    getZipMagic contents `Hspec.shouldBe` zipMagic

  Hspec.it "doesn't re-cache" . cdCache $ do
    atime1 <- cacheModel *> getModificationTime "mnist.ts.pt"
    atime2 <- cacheModel *> getModificationTime "mnist.ts.pt"
    atime2 `Hspec.shouldBe` atime1
  where
    cacheModel :: IO ()
    cacheModel =
      void . flip runReaderT env $
        traverse_ linkVersionedModel
          =<< (`getAndCacheModels` modelsWithIdents)
          =<< view (#config . #cache)

    clearCache :: IO ()
    clearCache =
      cdCache $
        traverse_ removeFile
          =<< listDirectory
          =<< getCurrentDirectory

    cdCache :: IO a -> IO a
    cdCache = env ^. #config . #cache . #path & withCurrentDirectory

modelsWithIdents :: Map Ident (Id ModelVersion, Text)
modelsWithIdents = Map.singleton "dummy" (mnistV1, "mnist")

mkDbSpec :: Env -> Spec
mkDbSpec env = Hspec.describe "Database" $ do
  Hspec.it "gets a model" $ do
    runReaderT (getModelsAndVersions models) env >>= \case
      v
        | Just (model, mversion) <- v ^? _head -> do
            view #name model `Hspec.shouldBe` "mnist"
            view (#version . to showVersion) mversion `Hspec.shouldBe` "v1"
            view #user model `Hspec.shouldBe` Nothing
        | otherwise -> Hspec.expectationFailure "No models were retrieved"

  Hspec.it "gets model size and contents" $ do
    getWithContents env >>= \case
      (size, contents) -> do
        -- This size is computed using PG functions
        size `Hspec.shouldBe` fromIntegral mnistV1Size
        -- This is the magic number for a ZIP
        getZipMagic contents `Hspec.shouldBe` zipMagic
        -- The size from PG and the size of the bytestring should be
        -- the same
        ByteString.length contents `Hspec.shouldBe` fromIntegral size

getWithContents :: Env -> IO (Integer, ByteString)
getWithContents env = flip runReaderT env $ do
  (getModelsAndVersions models >>=) . (. fmap snd . toList) $ \case
    [] -> throwString "No model was retrieved"
    v : _ -> getModelVersionSizeAndContents $ view #contents v

mnistV1 :: Id ModelVersion
mnistV1 = Id 1

models :: Vector (Id ModelVersion)
models = Vector.singleton mnistV1

mnistV1Size :: Int
mnistV1Size = 4808991

-- Magic header for a zip file (the underlying format of a `.ts.pt`)
zipMagic :: [Word8]
zipMagic = [80, 75, 3, 4]

getZipMagic :: ByteString -> [Word8]
getZipMagic = take 4 . ByteString.unpack
