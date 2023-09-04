{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Inferno.VersionControl.Testing (vcServerSpec) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Foreign.C (CTime (..))
import Inferno.Types.Syntax (Expr (Lit), Lit (LDouble), TV (TV))
import Inferno.Types.Type (ImplType (ImplType), TCScheme (ForallTC), typeDouble)
import Inferno.VersionControl.Client (ClientMWithVCStoreError, api, mkVCClientEnv)
import Inferno.VersionControl.Operations.Error (VCStoreError (..))
import Inferno.VersionControl.Server (VCServerError (VCServerError))
import Inferno.VersionControl.Types
  ( Pinned,
    VCMeta (..),
    VCObject (VCFunction),
    VCObjectHash,
    VCObjectPred (..),
    VCObjectVisibility (VCObjectPublic),
  )
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant ((:<|>) (..))
import Servant.Client (BaseUrl, ClientEnv, client)
import Servant.Typed.Error (runTypedClientM, typedClient)
import Test.Hspec
import Test.QuickCheck (Arbitrary, arbitrary, generate)

runOperation :: ClientEnv -> ClientMWithVCStoreError a -> IO a
runOperation vcClientEnv op =
  (flip runTypedClientM vcClientEnv op) >>= \case
    Left err -> do
      expectationFailure $ show err
      pure $ error "i shouldn't be evaluated"
    Right res ->
      pure res

runOperationFail :: (Show a) => ClientEnv -> ClientMWithVCStoreError a -> IO VCServerError
runOperationFail vcClientEnv op =
  (flip runTypedClientM vcClientEnv op) >>= \case
    Left (Right err) ->
      pure err
    Left (Left err) -> do
      expectationFailure $ "Expected VCServerError but failed with " <> show err
      pure $ error "i shouldn't be evaluated"
    Right res -> do
      expectationFailure $ "Expected this operation to fail but it returned " <> show res
      pure $ error "i shouldn't be evaluated"

createObj ::
  (Arbitrary a, Arbitrary g) =>
  VCObjectPred ->
  IO (VCMeta a g (Expr (Pinned VCObjectHash) (), TCScheme))
createObj predecessor = do
  g <- generate arbitrary
  createObjForGroup g predecessor

createObjForGroup ::
  (Arbitrary a) =>
  g ->
  VCObjectPred ->
  IO (VCMeta a g (Expr (Pinned VCObjectHash) (), TCScheme))
createObjForGroup group predecessor = do
  ctime <- CTime . round . toRational . utcTimeToPOSIXSeconds <$> getCurrentTime
  d <- generate arbitrary
  author <- generate arbitrary
  pure
    VCMeta
      { timestamp = ctime,
        author,
        group,
        name = "Test",
        description = "",
        Inferno.VersionControl.Types.pred = predecessor,
        visibility = VCObjectPublic,
        obj = (Lit () (LDouble d), ForallTC [TV 0] mempty $ ImplType mempty typeDouble)
      }

vcServerSpec ::
  forall g a.
  ( Arbitrary a,
    Show a,
    FromJSON a,
    ToJSON a,
    Arbitrary g,
    Show g,
    FromJSON g,
    ToJSON g
  ) =>
  BaseUrl ->
  Spec
vcServerSpec url = do
  vcClientEnv <- runIO $ do
    man <- newManager defaultManagerSettings
    pure $ mkVCClientEnv man url

  describe "inferno-vc server" $ do
    it "basics" $ do
      g <- generate arbitrary
      o1 <- createObjForGroup g Init
      h1 <- runOperation vcClientEnv (pushFunction o1)
      o2 <- createObjForGroup g $ MarkedBreakingWithPred h1
      h2 <- runOperation vcClientEnv (pushFunction o2)
      o3 <- createObjForGroup g $ MarkedBreakingWithPred h2
      h3 <- runOperation vcClientEnv (pushFunction o3)
      o4 <- createObjForGroup g $ MarkedBreakingWithPred h3
      h4 <- runOperation vcClientEnv (pushFunction o4)
      
      -- Test fetchFunction:
      forM_ [(o1, h1), (o2, h2), (o3, h3), (o4, h4)] $ \(o, h) -> do
        o' <-  runOperation vcClientEnv (fetchFunction h)
        timestamp o' `shouldBe` timestamp o
        obj o' `shouldBe` obj o

      -- Test fetchVCObject:
      forM_ [(o1, h1), (o2, h2), (o3, h3), (o4, h4)] $ \(o, h) -> do
        o' <-  runOperation vcClientEnv (fetchVCObject h)
        case obj o' of
          VCFunction e t -> do
            timestamp o' `shouldBe` timestamp o
            (e, t) `shouldBe` (obj o)
          _ -> expectationFailure "Expected to get a VCFunction"

      -- Test fetchVCObjects:
      hashToMeta <- runOperation vcClientEnv (fetchVCObjects [h1, h3, h4])
      Set.fromList (Map.keys hashToMeta) `shouldBe` Set.fromList [h1, h3, h4]
      forM_ [(o1, h1), (o3, h3), (o4, h4)] $ \(o, h) ->
        case Map.lookup h hashToMeta of
          Just meta ->
            timestamp meta `shouldBe` timestamp o
          Nothing -> expectationFailure "impossible"

      -- fetchFunctionsForGroups only returns the head h4:
      metas <- runOperation vcClientEnv (fetchFunctionsForGroups (Set.singleton g))
      map obj metas `shouldBe` [h4]

      -- The closure of h4 should be empty as it has no dependencies:
      metas' <- runOperation vcClientEnv (fetchVCObjectClosureHashes h4)
      metas' `shouldBe` []

    it "deletion" $ do
      o1 <- createObj Init
      h1 <- runOperation vcClientEnv (pushFunction o1)
      o2 <- createObj Init
      h2 <- runOperation vcClientEnv (pushFunction o2)
      o3 <- createObj Init
      h3 <- runOperation vcClientEnv (pushFunction o3)
      o4 <- createObj Init
      h4 <- runOperation vcClientEnv (pushFunction o4)
      runOperation vcClientEnv (deleteVCObject h3)
      -- Fetching h3 should fail:
      runOperationFail vcClientEnv (fetchFunction h3) >>= \case
        VCServerError (CouldNotFindPath _) -> pure ()
        _ -> expectationFailure ""
      -- Others should fetch:
      forM_ [(o1, h1), (o2, h2), (o4, h4)] $ \(o, h) -> do
        o' <- runOperation vcClientEnv (fetchFunction h)
        timestamp o' `shouldBe` timestamp o
        obj o' `shouldBe` obj o

    -- -- TODO is fetchFunctionsForGRoups wrong? It is returning deleted scripts:
    -- runOperation vcClientEnv (fetchFunctionsForGroups (Set.singleton 432)) $ \metas -> do
    --   Set.fromList (map obj metas) `shouldBe` Set.fromList [h4, h2, h1]

    it "deletion of autosave" $ do
      o1 <- createObj Init
      h1 <- runOperation vcClientEnv (pushFunction o1)
      o2 <- createObj Init
      h2 <- runOperation vcClientEnv (pushFunction (o2 {name = "<AUTOSAVE>"}))
      -- h1 isn't an autosave so can't delete it:
      runOperationFail vcClientEnv (deleteAutosavedFunction h1) >>= \case
        VCServerError (TryingToDeleteNonAutosave _) -> pure ()
        _ -> expectationFailure ""
      -- h2 is an autosave so it's fine
      runOperation vcClientEnv (deleteAutosavedFunction h2)

    it "history" $ do
      o1 <- createObj Init
      h1 <- runOperation vcClientEnv (pushFunction o1)
      o2 <- createObj $ MarkedBreakingWithPred h1
      h2 <- runOperation vcClientEnv (pushFunction o2)
      o3 <- createObj $ MarkedBreakingWithPred h2
      h3 <- runOperation vcClientEnv (pushFunction o3)
      o4 <- createObj $ MarkedBreakingWithPred h3
      h4 <- runOperation vcClientEnv (pushFunction o4)
      metas <- runOperation vcClientEnv (fetchVCObjectHistory h4)
      (map obj metas) `shouldBe` [h4, h3, h2, h1]

    it "history of clone" $ do
      o1 <- createObj Init
      h1 <- runOperation vcClientEnv (pushFunction o1)
      o2 <- createObj $ MarkedBreakingWithPred h1
      h2 <- runOperation vcClientEnv (pushFunction o2)
      o3 <- createObj $ CloneOf h2
      h3 <- runOperation vcClientEnv (pushFunction o3)
      o4 <- createObj $ MarkedBreakingWithPred h3
      h4 <- runOperation vcClientEnv (pushFunction o4)
      metas <- runOperation vcClientEnv (fetchVCObjectHistory h4)
      (map obj metas) `shouldBe` [h4, h3, h2]

    it "history of clone of clone" $ do
      o1 <- createObj Init
      h1 <- runOperation vcClientEnv (pushFunction o1)
      o2 <- createObj $ CloneOf h1
      h2 <- runOperation vcClientEnv (pushFunction o2)
      o3 <- createObj $ CloneOf h2
      h3 <- runOperation vcClientEnv (pushFunction o3)
      o4 <- createObj $ MarkedBreakingWithPred h3
      h4 <- runOperation vcClientEnv (pushFunction o4)
      metas <- runOperation vcClientEnv (fetchVCObjectHistory h4)
      (map obj metas) `shouldBe` [h4, h3, h2]

    it "history of clone of deleted" $ do
      o1 <- createObj Init
      h1 <- runOperation vcClientEnv (pushFunction o1)
      o2 <- createObj $ CloneOf h1
      h2 <- runOperation vcClientEnv (pushFunction o2)
      o3 <- createObj $ CloneOf h2
      runOperation vcClientEnv (deleteVCObject h2)
      h3 <- runOperation vcClientEnv (pushFunction o3)
      o4 <- createObj $ MarkedBreakingWithPred h3
      h4 <- runOperation vcClientEnv (pushFunction o4)
      metas <- runOperation vcClientEnv (fetchVCObjectHistory h4)
      (map obj metas) `shouldBe` [h4, h3, h2]
      let o3' = metas !! 1
      Inferno.VersionControl.Types.pred o3' `shouldBe` CloneOfRemoved h2
      -- Original object is not deleted
      metas' <- runOperation vcClientEnv (fetchVCObjectHistory h1)
      (map obj metas') `shouldBe` [h1]

    it "history of clone of deleted (clone is head)" $ do
      o1 <- createObj Init
      h1 <- runOperation vcClientEnv (pushFunction o1)
      o2 <- createObj $ CloneOf h1
      h2 <- runOperation vcClientEnv (pushFunction o2)
      o3 <- createObj $ CloneOf h2
      h3 <- runOperation vcClientEnv (pushFunction o3)
      runOperation vcClientEnv (deleteVCObject h2)
      metas <- runOperation vcClientEnv (fetchVCObjectHistory h3)
      (map obj metas) `shouldBe` [h3, h2]
      Inferno.VersionControl.Types.pred (metas !! 0) `shouldBe` CloneOfRemoved h2

    it "cannot branch" $ do
      o1 <- createObj Init
      h1 <- runOperation vcClientEnv (pushFunction o1)
      o2 <- createObj $ CompatibleWithPred h1
      _h2 <- runOperation vcClientEnv (pushFunction o2)
      o3 <- createObj $ CompatibleWithPred h1
      runOperationFail vcClientEnv (pushFunction o3) >>= \case
        VCServerError (TryingToAppendToNonHead _) -> pure ()
        _ -> expectationFailure ""


  where
    fetchFunction :: VCObjectHash -> ClientMWithVCStoreError (VCMeta a g (Expr (Pinned VCObjectHash) (), TCScheme))
    fetchFunctionsForGroups :: Set.Set g -> ClientMWithVCStoreError [VCMeta a g VCObjectHash]
    fetchVCObject :: VCObjectHash -> ClientMWithVCStoreError (VCMeta a g VCObject)
    fetchVCObjectHistory :: VCObjectHash -> ClientMWithVCStoreError [VCMeta a g VCObjectHash]
    fetchVCObjects :: [VCObjectHash] -> ClientMWithVCStoreError (Map.Map VCObjectHash (VCMeta a g VCObject))
    fetchVCObjectClosureHashes :: VCObjectHash -> ClientMWithVCStoreError [VCObjectHash]
    pushFunction :: VCMeta a g (Expr (Pinned VCObjectHash) (), TCScheme) -> ClientMWithVCStoreError VCObjectHash
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
      :<|> deleteVCObject = typedClient $ client $ api @a @g
