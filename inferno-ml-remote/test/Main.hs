module Main (main) where

import Control.Exception (Exception (displayException))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Generics.Product.Typed (typed)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Word (Word64)
import Inferno.ML.Remote.Client (runInference)
import Inferno.ML.Remote.Server (api, infernoMlRemote)
import Inferno.ML.Remote.Types
  ( EvalResult (EvalResult),
    InfernoMlRemoteEnv (InfernoMlRemoteEnv),
    InfernoMlRemoteError (CacheSizeExceeded),
    ModelCache (ModelCache),
    ModelCacheOption (CompressedPaths, Paths),
    Script (Script),
    SomeInfernoError,
  )
import Inferno.ML.Remote.Utils
  ( cacheAndUseModel,
    collectModelNames,
    mkFinalAst,
  )
import Inferno.Types.Syntax (Expr, SourcePos)
import Inferno.Types.VersionControl (VCObjectHash)
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
import System.Directory (listDirectory)
import System.FilePath (takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Arg, Spec, SpecWith)
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec $ do
  inferenceSpec
  Hspec.describe "Inferno.ML.Remote.Utils" $
    collectModelsSpec *> cacheModelsSpec

-- Tests `/inference` endpoint with various Inferno scripts with ML features
inferenceSpec :: Spec
inferenceSpec = do
  baseUrl <- Hspec.runIO $ parseBaseUrl "http://localhost"
  manager <- Hspec.runIO $ newManager defaultManagerSettings

  let mkEnv :: Int -> ClientEnv
      mkEnv port = baseUrl & typed @Int .~ port & mkClientEnv manager

      inferenceClient :: Script -> ClientM EvalResult
      inferenceClient = runInference api

      runClient :: Int -> ClientM a -> IO (Either ClientError a)
      runClient port = flip runClientM $ mkEnv port

      mkEvalTest :: FilePath -> EvalResult -> SpecWith (Arg (Int -> IO ()))
      mkEvalTest fp expected =
        Hspec.it ("evaluates " <> takeFileName fp) $ \port ->
          runClient port (inferenceClient =<< readScript fp)
            >>= either
              (Hspec.expectationFailure . show)
              (`Hspec.shouldBe` expected)

  Hspec.around (withTestAppAndEnv mkCacheOption) $ do
    Hspec.describe "POST /inference" $ do
      mkEvalTest "../inferno-ml/test/test.inferno" "Tensor Int64 []  262144"

      mkEvalTest "../inferno-ml/test/xor.inferno" . coerce . Text.strip $
        Text.unlines
          [ "[Tensor Float [1] [-6.7711e-5]",
            ",Tensor Float [1] [ 1.0000   ]",
            ",Tensor Float [1] [ 0.9999   ]",
            ",Tensor Float [1] [-1.9073e-6]]"
          ]

      mkEvalTest "../inferno-ml/test/test-cpu.inferno" "Tensor Float []  8.5899e9"

      mkEvalTest "../inferno-ml/test/mnist.inferno" "[[7]]"

  -- This tests caching and using the model with the filesystem compression
  -- option, the one above uses non-compressed paths
  Hspec.around (withTestAppAndEnv mkCompressedCacheOption) $ do
    Hspec.describe "POST /inference" $ do
      mkEvalTest "../inferno-ml/test/mnist.inferno" "[[7]]"

collectModelsSpec :: Spec
collectModelsSpec = Hspec.describe "collectModelNames" $ do
  Hspec.it "extracts models from script" $ do
    mkAstTest "../inferno-ml/test/mnist.inferno" $
      (`Hspec.shouldBe` ["mnist.ts.pt"]) . collectModelNames

  Hspec.it "does not extract superfluous models" $ do
    mkAstTest "../inferno-ml/test/xor.inferno" $
      (`Hspec.shouldBe` []) . collectModelNames

  Hspec.it "extracts multiple models from script" $ do
    mkAstTest "./test/contrived.inferno" $
      (`Hspec.shouldBe` ["x.ts.pt", "y.ts.pt"]) . collectModelNames

  Hspec.it "extracts a single model with non-literal name" $ do
    mkAstTest "./test/contrived4.inferno" $
      (`Hspec.shouldBe` ["x.ts.pt"]) . collectModelNames

  Hspec.it "extracts multiple models with non-literal names" $ do
    mkAstTest "./test/contrived2.inferno" $
      (`Hspec.shouldBe` ["x.ts.pt", "y.ts.pt"]) . collectModelNames

  Hspec.it "extracts multiple models with literal and non-literal names" $ do
    mkAstTest "./test/contrived2.inferno" $
      (`Hspec.shouldBe` ["x.ts.pt", "y.ts.pt"]) . collectModelNames
  where
    mkAstTest :: FilePath -> (Expr (Maybe VCObjectHash) SourcePos -> IO ()) -> IO ()
    mkAstTest fp f =
      either (Hspec.expectationFailure . displayException) f
        =<< astFromScript fp

    astFromScript ::
      FilePath -> IO (Either SomeInfernoError (Expr (Maybe VCObjectHash) SourcePos))
    astFromScript = fmap (fmap snd . mkFinalAst) . readScript

cacheModelsSpec :: Spec
cacheModelsSpec =
  Hspec.around withTempDir . Hspec.describe "cacheAndUseModel" $ do
    Hspec.it "caches a model" $ \fp -> do
      cacheAndUseModel "mnist.ts.pt" . mkCacheOption fp $ 10 * 1073741824
      (`Hspec.shouldBe` ["mnist.ts.pt"]) =<< listDirectory fp

    Hspec.it "decompresses and caches a model" $ \fp -> do
      cacheAndUseModel "mnist.ts.pt" . mkCompressedCacheOption fp $ 10 * 1073741824
      (`Hspec.shouldBe` ["mnist.ts.pt"]) =<< listDirectory fp

    Hspec.it "throws when model is too big" $ \fp -> do
      (`Hspec.shouldThrow` (== CacheSizeExceeded)) $
        cacheAndUseModel "mnist.ts.pt" (mkCacheOption fp 10)

withTestAppAndEnv ::
  (FilePath -> Word64 -> ModelCacheOption) -> (Int -> IO ()) -> IO ()
withTestAppAndEnv g f =
  withTempDir $
    (`withTestApp` f)
      . InfernoMlRemoteEnv
      . Just
      . (`g` maxSize)
  where
    maxSize :: Word64
    maxSize = 10 * 1073741824

mkCacheOption :: FilePath -> Word64 -> ModelCacheOption
mkCacheOption fp = Paths "./test" . ModelCache fp

mkCompressedCacheOption :: FilePath -> Word64 -> ModelCacheOption
mkCompressedCacheOption fp = CompressedPaths "./test" . ModelCache fp

withTempDir :: (FilePath -> IO ()) -> IO ()
withTempDir = withSystemTempDirectory "inferno-ml-remote-tests"

withTestApp :: InfernoMlRemoteEnv -> (Int -> IO ()) -> IO ()
withTestApp = testWithApplication . pure . infernoMlRemote

readScript :: MonadIO m => FilePath -> m Script
readScript = fmap coerce . liftIO . Text.IO.readFile
