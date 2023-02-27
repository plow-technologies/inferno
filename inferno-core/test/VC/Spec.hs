{-# LANGUAGE DataKinds #-}

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
import Network.HTTP.Client (responseStatus, Response (..), httpLbs, Request, RequestBody (..), host, method, path, port, queryString, requestBody, requestHeaders, secure, defaultRequest)
import Data.String.Conversions  (cs)
import Network.HTTP.Types (status500)
import Plow.Logging (IOTracer (..), simpleStdOutTracer)
import Servant.Server (Server)
import Servant.QuickCheck
import Servant.QuickCheck.Internal.ErrorTypes (PredicateFailure (..))
import Test.Hspec
import Inferno.VersionControl.Operations.Error (VCStoreError(..))
import GHC.Utils.TmpFs (withSystemTempDirectory)
import System.Directory (removePathForcibly)
import Test.QuickCheck (Result(..), quickCheckWithResult, Gen, Arbitrary (..), elements)
import Control.Concurrent (newEmptyMVar, tryPutMVar, tryReadMVar)
import Servant.QuickCheck.Internal.HasGenRequest (runGenRequest, HasGenRequest (..))
import Test.QuickCheck.Monadic (monadicIO, forAllM, run, assert)
import Servant.QuickCheck.Internal.Predicates (finishPredicates)
import Servant.QuickCheck.Internal.QuickCheck (noCheckStatus, defManager)
import Control.Concurrent.Async (concurrently)
import Data.Proxy (Proxy (..))
import Inferno.Types.Type (TCScheme)
import Inferno.VersionControl.Types (VCObjectHash, Pinned, VCMeta)
import Inferno.Types.Syntax (Expr)
import Servant.API.ContentTypes (JSON (..), AllMimeRender (allMimeRender))
import Network.HTTP.Media       (renderHeader)

newtype VCApi = VCApi (VersionControlAPI Int Int)

-- Test set: store successive versions of the same script, run fetchHistory for arbitrary versions
-- Try to trigger CouldNotFindPath when fetchHistory reads wrong head
instance HasGenRequest (VCApi) where
  genRequest _ = (1, genStore)
    where
      genStore :: Gen (BaseUrl -> Request)
      genStore = do
        new' <- new
        (ct, bd) <- elements $ allMimeRender (Proxy :: Proxy '[JSON]) new'
        return $ \burl -> defaultRequest
          { host = cs $ baseUrlHost burl
          , port = baseUrlPort burl
          , secure = baseUrlScheme burl == Https
          , method = "POST"
          , path = "/push/function"
          , requestBody = RequestBodyLBS bd
          , requestHeaders = ("Content-Type", renderHeader ct) : []
          }
        where
          -- TODO keep a list and point to correct pred
          -- Do we need MTL for this? See GenT
          -- TODO limit size of Expr
          new = arbitrary :: Gen (VCMeta Int Int (Expr (Pinned VCObjectHash) (), TCScheme))
      genFetchHist :: Gen (BaseUrl -> Request)
      genFetchHist = return $ \burl -> defaultRequest
        { host = cs $ baseUrlHost burl
        , port = baseUrlPort burl
        , secure = baseUrlScheme burl == Https
        , method = "GET"
        , path = "/fetch/X8-B10K4IF4blrwGl3oztxlw-0LAeiQvkx5bxvLrr4Y="
        }

noInternalError :: RequestPredicate
noInternalError = RequestPredicate $ \req mgr -> do
  resp <- httpLbs req mgr
  putStrLn $ show req ++ show resp
  when (responseStatus resp == status500) $ do
    case ((Aeson.decode $ responseBody resp) :: Maybe VCStoreError) of
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
  let maxTries = 20
  let args = defaultArgs { maxSuccess = maxTries }
  -- let args = defaultArgs

  -- it "fixed store dir" $ do
  --   let vcPath = "vc_store"
  --   removePathForcibly vcPath
  --   withServantServer testApi (pserver vcPath) $ \url ->
  --     serverSatisfies testApi url args (noInternalError <%> mempty)

  -- TODO switch to using temp dir
  -- it "no internal errors" $ do
  --   withSystemTempDirectory "vc_store_" $ \vcPath -> do
  --     withServantServer testApi (pserver vcPath) $ \url ->
  --       serverSatisfies testApi url args (noInternalError <%> mempty)
  --       -- TODO this works. Is the problem our predicate?
  --       -- serverSatisfies testApi url args (not500 <%> mempty)
  --   putStrLn "Done"

  it "concurrent test" $ do
    let preds = noInternalError <%> mempty
    let vcPath = "vc_store"
    removePathForcibly vcPath
    let proxyApi = Proxy :: Proxy VCApi

    withServantServer testApi (pserver vcPath) $ \burl -> do
      deetsMVar <- newEmptyMVar
      let reqs = ($ burl) <$> runGenRequest proxyApi
      let prop = monadicIO $ forAllM reqs $ \req -> do
            v <- run $ finishPredicates preds (noCheckStatus req) defManager
            _ <- run $ tryPutMVar deetsMVar v
            case v of
              Just _ -> assert False
              _ -> return ()
      -- TODO do both threads generate the exact same sequence of requests?
      (res1, res2) <- concurrently
        (quickCheckWithResult args {chatty = False} prop)
        (quickCheckWithResult args {chatty = False} prop)
      -- TODO need a way to kill other threads when property violated in one thread
      case (res1, res2) of
        (Success {}, Success {}) -> return ()
        _ -> do
          mx <- tryReadMVar deetsMVar
          case mx of
            Just x ->
              expectationFailure $ "Failed:\n" ++ show x
            Nothing ->
              expectationFailure $ "We failed to record a reason for failure: " <> show (res1, res2)

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
