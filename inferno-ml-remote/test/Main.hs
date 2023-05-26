module Main (main) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Generics.Product.Typed (typed)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Inferno.ML.Remote.Client (runInference)
import Inferno.ML.Remote.Server (api, infernoMlRemote)
import Inferno.ML.Remote.Types (EvalResult (EvalResult), InfernoMlRemoteEnv (InfernoMlRemoteEnv), Script (Script))
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

      inferenceClient :: Script -> ClientM EvalResult
      inferenceClient = runInference api

      runClient :: Int -> ClientM a -> IO (Either ClientError a)
      runClient port = flip runClientM (mkEnv port)

      mkInferenceTest :: FilePath -> EvalResult -> SpecWith (Arg (Int -> IO ()))
      mkInferenceTest fp expected =
        Hspec.it ("evaluates " <> takeFileName fp) $ \port ->
          runClient port (inferenceClient =<< readScript fp)
            >>= either
              (Hspec.expectationFailure . show)
              (`Hspec.shouldBe` expected)

  Hspec.describe "POST /inference" $ do
    mkInferenceTest "../inferno-ml/test/test.inferno" "Tensor Int64 []  262144"

    mkInferenceTest "../inferno-ml/test/xor.inferno" . coerce . Text.strip $
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
withTestApp = testWithApplication . pure $ infernoMlRemote env
  where
    env :: InfernoMlRemoteEnv
    env = InfernoMlRemoteEnv Nothing

readScript :: MonadIO m => FilePath -> m Script
readScript = fmap coerce . liftIO . Text.IO.readFile
