module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Generics.Product.Typed (typed)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Inferno.ML.Remote.Client (runInference)
import Inferno.ML.Remote.Server (infernoMlRemote, infernoMlRemoteAPI)
import Lens.Micro.Platform ((.~))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.Client
  ( ClientEnv,
    ClientError,
    ClientM,
    mkClientEnv,
    parseBaseUrl,
    runClientM,
  )
import System.FilePath (takeFileName)
import Test.Hspec (Arg, Spec, SpecWith)
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec inferenceSpec

-- Tests `/inference` endpoint with various Inferno scripts with ML features
inferenceSpec :: Spec
inferenceSpec = Hspec.around withTestApp $ do
  baseUrl <- Hspec.runIO $ parseBaseUrl "http://localhost"
  manager <- Hspec.runIO $ newManager defaultManagerSettings

  let mkEnv :: Int -> ClientEnv
      mkEnv port = baseUrl & typed @Int .~ port & mkClientEnv manager

      inferenceClient :: Text -> ClientM Text
      inferenceClient = runInference infernoMlRemoteAPI

      runClient :: Int -> ClientM a -> IO (Either ClientError a)
      runClient port = flip runClientM (mkEnv port)

      mkInferenceTest :: FilePath -> Text -> SpecWith (Arg (Int -> IO ()))
      mkInferenceTest fp expected =
        Hspec.it ("evaluates " <> takeFileName fp) $ \port ->
          runClient port (inferenceClient =<< liftIO (Text.IO.readFile fp))
            >>= \case
              Left e -> Hspec.expectationFailure $ show e
              Right x -> x `Hspec.shouldBe` expected

  Hspec.describe "POST /inference" $ do
    mkInferenceTest "../inferno-ml/test/test.inferno" "Tensor Int64 []  262144"

    mkInferenceTest "../inferno-ml/test/xor.inferno" . Text.strip $
      Text.unlines
        [ "[Tensor Float [1] [-6.7711e-5]",
          ",Tensor Float [1] [ 1.0000   ]",
          ",Tensor Float [1] [ 0.9999   ]",
          ",Tensor Float [1] [-1.9073e-6]]"
        ]

    mkInferenceTest
      "../inferno-ml/test/test-cpu.inferno"
      "Tensor Float []  8.5899e9   "

withTestApp :: (Int -> IO ()) -> IO ()
withTestApp = testWithApplication (pure infernoMlRemote)
