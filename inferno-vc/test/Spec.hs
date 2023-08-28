{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (forM_)
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Foreign.C (CTime (..))
import Inferno.Types.Syntax (Expr (Lit), Lit (LDouble), TV (TV))
import Inferno.Types.Type (ImplType (ImplType), TCScheme (ForallTC), typeDouble)
import Inferno.VersionControl.Client (ClientMWithVCStoreError, api, mkVCClientEnv)
import Inferno.VersionControl.Operations.Error (VCStoreError (..))
import qualified Inferno.VersionControl.Operations.Filesystem as FSOps
import Inferno.VersionControl.Server (VCServerError (VCServerError), runServerConfig)
import Inferno.VersionControl.Server.Types (ServerConfig (..))
import Inferno.VersionControl.Types (Pinned, VCMeta (..), VCObject (VCFunction), VCObjectHash, VCObjectPred (CloneOf, Init, MarkedBreakingWithPred), VCObjectVisibility (VCObjectPublic))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant ((:<|>) (..))
import Servant.Client (BaseUrl (..), ClientEnv, Scheme (..), client)
import Servant.Typed.Error (runTypedClientM, typedClient)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.QuickCheck (arbitrary, generate)

fetchFunction :: VCObjectHash -> ClientMWithVCStoreError (VCMeta Int Int (Expr (Pinned VCObjectHash) (), TCScheme))
fetchFunctionsForGroups :: Set.Set Int -> ClientMWithVCStoreError [VCMeta Int Int VCObjectHash]
fetchVCObject :: VCObjectHash -> ClientMWithVCStoreError (VCMeta Int Int VCObject)
fetchVCObjectHistory :: VCObjectHash -> ClientMWithVCStoreError [VCMeta Int Int VCObjectHash]
fetchVCObjects :: [VCObjectHash] -> ClientMWithVCStoreError (Map.Map VCObjectHash (VCMeta Int Int VCObject))
fetchVCObjectClosureHashes :: VCObjectHash -> ClientMWithVCStoreError [VCObjectHash]
pushFunction :: VCMeta Int Int (Expr (Pinned VCObjectHash) (), TCScheme) -> ClientMWithVCStoreError VCObjectHash
deleteAutosavedFunction :: VCObjectHash -> ClientMWithVCStoreError ()
deleteVCObject :: VCObjectHash -> ClientMWithVCStoreError ()
fetchFunction
  :<|> fetchFunctionsForGroups
  :<|> fetchVCObject
  :<|> fetchVCObjectHistory
  :<|> fetchVCObjects
  :<|> fetchVCObjectClosureHashes
  :<|> pushFunction
  :<|> deleteAutosavedFunction
  :<|> deleteVCObject = typedClient $ client $ api @Int @Int

runOperation :: ClientEnv -> ClientMWithVCStoreError a -> (a -> IO ()) -> IO ()
runOperation vcClientEnv op check = do
  (flip runTypedClientM vcClientEnv op) >>= \case
    Left err -> do
      expectationFailure $ show err
    Right res -> do
      check res

runOperationFail :: (Show a) => ClientEnv -> ClientMWithVCStoreError a -> (VCServerError -> IO ()) -> IO ()
runOperationFail vcClientEnv op check = do
  (flip runTypedClientM vcClientEnv op) >>= \case
    Left (Right err) -> do
      check err
    Left (Left err) -> do
      expectationFailure $ "Expected VCServerError but failed with " <> show err
    Right res -> do
      expectationFailure $ "Expected this operation to fail but it returned " <> show res

createObj :: VCObjectPred -> IO (VCMeta Int Int (Expr (Pinned VCObjectHash) (), TCScheme))
createObj predecessor = do
  ctime <- CTime . round . toRational . utcTimeToPOSIXSeconds <$> getCurrentTime
  d <- generate arbitrary
  pure
    VCMeta
      { timestamp = ctime,
        author = 432,
        group = 432,
        name = "Test",
        description = "",
        Inferno.VersionControl.Types.pred = predecessor,
        visibility = VCObjectPublic,
        obj = (Lit () (LDouble d), ForallTC [TV 0] mempty $ ImplType mempty typeDouble)
      }

spec :: ClientEnv -> Spec
spec vcClientEnv =
  describe "inferno-vc server" $ do
    it "basics" $ do
      o1 <- createObj Init
      runOperation vcClientEnv (pushFunction o1) $ \h1 -> do
        o2 <- createObj $ MarkedBreakingWithPred h1
        runOperation vcClientEnv (pushFunction o2) $ \h2 -> do
          o3 <- createObj $ MarkedBreakingWithPred h2
          runOperation vcClientEnv (pushFunction o3) $ \h3 -> do
            o4 <- createObj $ MarkedBreakingWithPred h3
            runOperation vcClientEnv (pushFunction o4) $ \h4 -> do
              -- Test fetchFunction:
              forM_ [(o1, h1), (o2, h2), (o3, h3), (o4, h4)] $ \(o, h) ->
                runOperation vcClientEnv (fetchFunction h) $ \o' -> do
                  timestamp o' `shouldBe` timestamp o
                  obj o' `shouldBe` obj o

              -- Test fetchVCObject:
              forM_ [(o1, h1), (o2, h2), (o3, h3), (o4, h4)] $ \(o, h) ->
                runOperation vcClientEnv (fetchVCObject h) $ \o' ->
                  case obj o' of
                    VCFunction e t -> do
                      timestamp o' `shouldBe` timestamp o
                      (e, t) `shouldBe` (obj o)
                    _ -> expectationFailure "Expected to get a VCFunction"

              -- Test fetchVCObjects:
              runOperation vcClientEnv (fetchVCObjects [h1, h3, h4]) $ \hashToMeta -> do
                Set.fromList (Map.keys hashToMeta) `shouldBe` Set.fromList [h1, h3, h4]
                forM_ [(o1, h1), (o3, h3), (o4, h4)] $ \(o, h) ->
                  case Map.lookup h hashToMeta of
                    Just meta ->
                      timestamp meta `shouldBe` timestamp o
                    Nothing -> expectationFailure "impossible"

              -- fetchFunctionsForGroups only returns the head h4:
              runOperation vcClientEnv (fetchFunctionsForGroups (Set.singleton 432)) $ \metas -> do
                map obj metas `shouldBe` [h4]

              -- The closure of h4 should be empty as it has no dependencies:
              runOperation vcClientEnv (fetchVCObjectClosureHashes h4) $ \metas -> do
                metas `shouldBe` []

    it "deletion" $ do
      o1 <- createObj Init
      runOperation vcClientEnv (pushFunction o1) $ \h1 -> do
        o2 <- createObj Init
        runOperation vcClientEnv (pushFunction o2) $ \h2 -> do
          o3 <- createObj Init
          runOperation vcClientEnv (pushFunction o3) $ \h3 -> do
            o4 <- createObj Init
            runOperation vcClientEnv (pushFunction o4) $ \h4 -> do
              runOperation vcClientEnv (deleteVCObject h3) $ \() -> do
                -- Fetching h3 should fail:
                runOperationFail vcClientEnv (fetchFunction h3) $ \case
                  VCServerError (CouldNotFindPath _) -> pure ()
                  _ -> expectationFailure ""
                -- Others should fetch:
                forM_ [(o1, h1), (o2, h2), (o4, h4)] $ \(o, h) ->
                  runOperation vcClientEnv (fetchFunction h) $ \o' -> do
                    timestamp o' `shouldBe` timestamp o
                    obj o' `shouldBe` obj o

    -- -- TODO is fetchFunctionsForGRoups wrong? It is returning deleted scripts:
    -- runOperation vcClientEnv (fetchFunctionsForGroups (Set.singleton 432)) $ \metas -> do
    --   Set.fromList (map obj metas) `shouldBe` Set.fromList [h4, h2, h1]

    it "deletion of autosave" $ do
      o1 <- createObj Init
      runOperation vcClientEnv (pushFunction o1) $ \h1 -> do
        o2 <- createObj Init
        runOperation vcClientEnv (pushFunction (o2 {name = "<AUTOSAVE>"})) $ \h2 -> do
          -- h1 isn't an autosave so can't delete it:
          runOperationFail vcClientEnv (deleteAutosavedFunction h1) $ \case
            VCServerError (TryingToDeleteNonAutosave _) -> pure ()
            _ -> expectationFailure ""
          -- h2 is an autosave so it's fine
          runOperation vcClientEnv (deleteAutosavedFunction h2) $ \() -> pure ()

    it "history" $ do
      o1 <- createObj Init
      runOperation vcClientEnv (pushFunction o1) $ \h1 -> do
        o2 <- createObj $ MarkedBreakingWithPred h1
        runOperation vcClientEnv (pushFunction o2) $ \h2 -> do
          o3 <- createObj $ MarkedBreakingWithPred h2
          runOperation vcClientEnv (pushFunction o3) $ \h3 -> do
            o4 <- createObj $ MarkedBreakingWithPred h3
            runOperation vcClientEnv (pushFunction o4) $ \h4 -> do
              runOperation vcClientEnv (fetchVCObjectHistory h4) $ \metas ->
                (map obj metas) `shouldBe` [h4, h3, h2, h1]

    it "history of clone" $ do
      o1 <- createObj Init
      runOperation vcClientEnv (pushFunction o1) $ \h1 -> do
        o2 <- createObj $ MarkedBreakingWithPred h1
        runOperation vcClientEnv (pushFunction o2) $ \h2 -> do
          o3 <- createObj $ CloneOf h2
          runOperation vcClientEnv (pushFunction o3) $ \h3 -> do
            o4 <- createObj $ MarkedBreakingWithPred h3
            runOperation vcClientEnv (pushFunction o4) $ \h4 -> do
              runOperation vcClientEnv (fetchVCObjectHistory h4) $ \metas ->
                (map obj metas) `shouldBe` [h4, h3, h2]

    it "history of clone of clone" $ do
      o1 <- createObj Init
      runOperation vcClientEnv (pushFunction o1) $ \h1 -> do
        o2 <- createObj $ CloneOf h1
        runOperation vcClientEnv (pushFunction o2) $ \h2 -> do
          o3 <- createObj $ CloneOf h2
          runOperation vcClientEnv (pushFunction o3) $ \h3 -> do
            o4 <- createObj $ MarkedBreakingWithPred h3
            runOperation vcClientEnv (pushFunction o4) $ \h4 -> do
              runOperation vcClientEnv (fetchVCObjectHistory h4) $ \metas ->
                (map obj metas) `shouldBe` [h4, h3, h2]

main :: IO ()
main =
  withSystemTempDirectory "vc_store_" $ \vcPath -> do
    putStrLn $ "Store is at: " ++ (show vcPath)

    putStr "Starting Inferno VC..."
    _ <-
      forkIO $
        runServerConfig
          (Proxy :: Proxy Int)
          (Proxy :: Proxy Int)
          FSOps.initVCStore
          FSOps.runInfernoVCFilesystemM
          ServerConfig
            { _serverHost = "127.0.0.1",
              _serverPort = 13077,
              _vcPath = vcPath
            }
    man <- newManager defaultManagerSettings
    let vcClientEnv =
          mkVCClientEnv man $
            BaseUrl
              { baseUrlScheme = Http,
                baseUrlHost = "127.0.0.1",
                baseUrlPort = 13077,
                baseUrlPath = []
              }
    putStrLn "  Done."

    hspec $ spec vcClientEnv
