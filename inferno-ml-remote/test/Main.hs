module Main (main) where

import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Generics.Product.Typed (typed)
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import Inferno.ML.Module.Prelude (zerosFun)
import Inferno.ML.Remote.Client (runInference)
import Inferno.ML.Remote.Server (infernoMlRemote, infernoMlRemoteAPI)
import Inferno.ML.Types.Value (MlValue (VTensor))
import Inferno.Utils.Prettyprinter (renderPretty)
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
import Test.Hspec (Spec)
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec inferenceSpec

inferenceSpec :: Spec
inferenceSpec = Hspec.around withTestApp $ do
  baseUrl <- Hspec.runIO $ parseBaseUrl "http://localhost"
  manager <- Hspec.runIO $ newManager defaultManagerSettings
  script <- Hspec.runIO $ Text.IO.readFile "./test/test.inferno"

  let mkEnv :: Int -> ClientEnv
      mkEnv port = baseUrl & typed @Int .~ port & mkClientEnv manager

      inferenceClient :: Text -> ClientM Text
      inferenceClient = runInference infernoMlRemoteAPI

      runClient :: Int -> ClientM a -> IO (Either ClientError a)
      runClient port = flip runClientM (mkEnv port)

  Hspec.describe "POST /inference" $ do
    Hspec.it "evaluates Inferno script" $ \port -> do
      runClient port (inferenceClient script) >>= \case
        Left e -> Hspec.expectationFailure $ show e
        Right x -> x `Hspec.shouldBe` prettyZeros
  where
    prettyZeros :: Text
    prettyZeros = renderPretty . VTensor $ zerosFun [3, 4]

withTestApp :: (Int -> IO ()) -> IO ()
withTestApp = testWithApplication (pure infernoMlRemote)
