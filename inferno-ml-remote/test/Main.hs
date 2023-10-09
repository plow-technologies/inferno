module Main (main) where

import Control.Monad.Reader (runReaderT)
import Data.Aeson (eitherDecodeFileStrict)
import qualified Data.ByteString as ByteString
import Inferno.ML.Remote.Handler.Inference.Model (getModel, getModelContents)
import Inferno.ML.Remote.Server (runInEnv)
import Inferno.ML.Remote.Types
import Lens.Micro.Platform
import Test.Hspec (Spec)
import qualified Test.Hspec as Hspec
import UnliftIO (throwString)
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

mkDbSpec :: Env -> Spec
mkDbSpec env = Hspec.describe "Database" $ do
  Hspec.it "gets a model" $ do
    model <- flip runReaderT env . getModel $ RequestedModel "mnist" "v1"
    view #name model `Hspec.shouldBe` "mnist"
    view #version model `Hspec.shouldBe` "v1"
    view #user model `Hspec.shouldBe` Nothing

  Hspec.it "gets model size and contents" $ do
    (size, contents) <-
      flip runReaderT env $
        getModelContents . view #contents
          =<< getModel (RequestedModel "mnist" "v1")
    size `Hspec.shouldBe` 4808991
    -- This is the magic number for a ZIP
    (take 4 . ByteString.unpack) contents `Hspec.shouldBe` [80, 75, 3, 4]
    -- The size from PG and the size of the bytestring should be the same
    ByteString.length contents `Hspec.shouldBe` fromIntegral size
