{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import qualified Data.Aeson as Aeson
import Control.Exception (throw)
import Control.Monad.Reader (runReaderT, when)
import Data.Functor.Contravariant (contramap)
import Inferno.Instances.Arbitrary ()
import Inferno.VersionControl.Client (api)
import Inferno.VersionControl.Log (vcServerTraceToString)
import qualified Inferno.VersionControl.Operations as Ops
import Inferno.VersionControl.Server (VersionControlAPI, vcServer)
import Network.HTTP.Client (responseStatus, Response (..), httpLbs)
import Network.HTTP.Types (status500)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Handler.Warp (defaultSettings, setLogger)
import Plow.Logging (IOTracer (..), simpleStdOutTracer)
import Servant.Server (Server)
import Servant.QuickCheck
import Servant.QuickCheck.Internal.ErrorTypes (PredicateFailure (..))
import Test.Hspec
import Inferno.VersionControl.Operations.Error (VCStoreError(..))
import GHC.Utils.TmpFs (withSystemTempDirectory)

noInternalError :: RequestPredicate
noInternalError = RequestPredicate $ \req mgr -> do
  resp <- httpLbs req mgr
  putStrLn $ show req ++ show resp
  when (responseStatus resp == status500) $ do
    case (Aeson.decode $ responseBody resp :: Maybe VCStoreError) of
      -- HACK: these are the non-errors, which should be 404s or other codes:
      Just (CouldNotFindObject _) -> pure ()
      Just (TryingToAppendToNonHead _) -> pure ()
      Just (TryingToDeleteNonAutosave _) -> pure ()
      _ -> do
        putStrLn "BOO"
        throw $ PredicateFailure "noInternalError" (Just req) resp
  return [resp]

spec :: Spec
spec = describe "inferno-vc server" $ do
  -- let maxTries = 20
  -- let args = defaultArgs { maxSuccess = maxTries }

  it "no internal errors" $ withStdoutLogger $ \appLogger -> do
    withSystemTempDirectory "vc_store_" $ \vcPath -> do
      -- let settings = setLogger appLogger defaultSettings
      let settings = defaultSettings
      withServantServerAndSettings testApi settings (pserver vcPath) $ \url ->
        serverSatisfies testApi url defaultArgs (noInternalError <%> mempty)
      -- putStrLn "Done"

  where
    tracer = contramap vcServerTraceToString $ IOTracer $ simpleStdOutTracer

    testApi :: Proxy (VersionControlAPI Int Int)
    testApi = api

    pserver :: FilePath -> IO (Server (VersionControlAPI Int Int))
    pserver vcPath = do
      putStrLn $ "Store is at: " ++ (show vcPath)
      runReaderT Ops.initVCStore $ Ops.VCStorePath vcPath
      print ("running..." :: String)
      return $ vcServer vcPath tracer

main :: IO ()
main =
  hspec spec
