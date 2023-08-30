{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Control.Concurrent.MVar as MVar
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant.Client (ClientM, client)
import Servant.ConcTest (Method (..), isLinearizable, withServantServer)
import Test.QuickCheck (elements, forAll, ioProperty, quickCheck, vectorOf, verbose)

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

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  -- Define the API to be tested
  let putClientFn = Method {Servant.ConcTest.name = "put", clientFn = show <$> putClient}
  let getClientFn = Method {Servant.ConcTest.name = "get", clientFn = show <$> getClient}
  let fns = [getClientFn, putClientFn]

  -- Generate concurrent executions to be tested
  let numThreads = 2
  let numCalls = 2
  let execGen = vectorOf numThreads $ vectorOf numCalls $ elements fns

  withServantServer api server $ \burl -> do
    quickCheck $
      verbose $
        forAll execGen (ioProperty . isLinearizable manager burl resetClient)
