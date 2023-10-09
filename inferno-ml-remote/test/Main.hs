module Main (main) where

import Control.Monad (void)
import Control.Monad.Reader (asks, runReaderT)
import Data.Aeson (eitherDecodeFileStrict)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (traverse_)
import Data.Word (Word8)
import Inferno.ML.Remote.Handler.Inference
  ( getAndCacheModel,
    linkVersionedModel,
  )
import Inferno.ML.Remote.Handler.Inference.Model
  ( getModel,
    getModelContents,
  )
import Inferno.ML.Remote.Server (runInEnv)
import Inferno.ML.Remote.Types
import Lens.Micro.Platform
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
        =<< either throwString pure
        =<< eitherDecodeFileStrict @Options cfg
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
    dir <- env ^. #cache . #path & listDirectory
    dir `Hspec.shouldContain` ["mnist.v1", "mnist.ts.pt"]
    contents <- ByteString.readFile "mnist.ts.pt"
    ByteString.length contents `Hspec.shouldBe` mnistSize
    getZipMagic contents `Hspec.shouldBe` zipMagic

  Hspec.it "doesn't re-cache" . cdCache $ do
    atime1 <- cacheModel *> getModificationTime "mnist.ts.pt"
    atime2 <- cacheModel *> getModificationTime "mnist.ts.pt"
    atime2 `Hspec.shouldBe` atime1
  where
    cacheModel :: IO ()
    cacheModel =
      void . flip runReaderT env $
        linkVersionedModel
          =<< (`getAndCacheModel` mnist)
          =<< asks (view #cache)

    clearCache :: IO ()
    clearCache =
      cdCache $
        traverse_ removeFile
          =<< listDirectory
          =<< getCurrentDirectory

    cdCache :: IO a -> IO a
    cdCache = env ^. #cache . #path & withCurrentDirectory

mkDbSpec :: Env -> Spec
mkDbSpec env = Hspec.describe "Database" $ do
  Hspec.it "gets a model" $ do
    model <- flip runReaderT env $ getModel mnist
    view #name model `Hspec.shouldBe` "mnist"
    view #version model `Hspec.shouldBe` "v1"
    view #user model `Hspec.shouldBe` Nothing

  Hspec.it "gets model size and contents" $ do
    (size, contents) <-
      flip runReaderT env $
        getModelContents . view #contents
          =<< getModel mnist
    -- This size is computed using PG functions
    size `Hspec.shouldBe` fromIntegral mnistSize
    -- This is the magic number for a ZIP
    getZipMagic contents `Hspec.shouldBe` zipMagic
    -- The size from PG and the size of the bytestring should be the same
    ByteString.length contents `Hspec.shouldBe` fromIntegral size

mnist :: RequestedModel
mnist = RequestedModel "mnist" "v1"

mnistSize :: Int
mnistSize = 4808991

zipMagic :: [Word8]
zipMagic = [80, 75, 3, 4]

getZipMagic :: ByteString -> [Word8]
getZipMagic = take 4 . ByteString.unpack
