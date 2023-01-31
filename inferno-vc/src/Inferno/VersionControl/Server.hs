{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Inferno.VersionControl.Server where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (link, withAsync)
import           Control.Concurrent.FairRWLock        ( RWLock )
import qualified Control.Concurrent.FairRWLock as RWL
import Control.Monad (forever)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Contravariant (contramap)
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.String (fromString)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Inferno.Types.Syntax (Expr)
import Inferno.Types.Type (TCScheme)
import Inferno.VersionControl.Log (VCServerTrace (ThrownVCStoreError), vcServerTraceToString)
import qualified Inferno.VersionControl.Operations as Ops
import qualified Inferno.VersionControl.Operations.Error as Ops
import Inferno.VersionControl.Server.Types (ServerConfig (..), readServerConfig)
import Inferno.VersionControl.Server.UnzipRequest (ungzipRequest)
import Inferno.VersionControl.Types
  ( Pinned,
    VCHashUpdate,
    VCMeta (..),
    VCObject (..),
    VCObjectHash,
    showVCObjectType,
  )
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setHost,
    setPort,
    setTimeout,
  )
import Network.Wai.Middleware.Gzip (def, gzip)
import Plow.Logging (IOTracer (..), simpleStdOutTracer, traceWith)
import Servant.API (Capture, JSON, ReqBody, Union, (:<|>) (..), (:>))
import Servant.Server (Handler, Server, serve)
import Servant.Typed.Error
  ( DeleteTypedError,
    GetTypedError,
    PostTypedError,
    WithError,
    liftTypedError,
  )

newtype VCServerError = VCServerError {serverError :: Ops.VCStoreError}
  deriving (Generic, Show)
  deriving newtype (ToJSON, FromJSON)

type GetThrowingVCStoreError resp ty = GetTypedError resp ty VCServerError

type PostThrowingVCStoreError resp ty = PostTypedError resp ty VCServerError

type DeleteThrowingVCStoreError resp ty = DeleteTypedError resp ty VCServerError

type VersionControlAPI a g =
  "fetch" :> "function" :> Capture "hash" VCObjectHash :> GetThrowingVCStoreError '[JSON] (VCMeta a g (Expr (Pinned VCObjectHash) (), TCScheme))
    :<|> "fetch" :> "functions" :> ReqBody '[JSON] (Set g) :> PostThrowingVCStoreError '[JSON] [VCMeta a g VCObjectHash]
    :<|> "fetch" :> Capture "hash" VCObjectHash :> GetThrowingVCStoreError '[JSON] (VCMeta a g VCObject)
    :<|> "fetch" :> Capture "hash" VCObjectHash :> "history" :> GetThrowingVCStoreError '[JSON] [VCMeta a g VCObjectHash]
    :<|> "fetch" :> "objects" :> ReqBody '[JSON] [VCObjectHash] :> PostThrowingVCStoreError '[JSON] (Map.Map VCObjectHash (VCMeta a g VCObject))
    :<|> "fetch" :> "object" :> Capture "hash" VCObjectHash :> "closure" :> "hashes" :> GetThrowingVCStoreError '[JSON] [VCObjectHash]
    :<|> "push" :> "function" :> ReqBody '[JSON] (VCMeta a g (Expr (Pinned VCObjectHash) (), TCScheme)) :> PostThrowingVCStoreError '[JSON] VCObjectHash
    :<|> "delete" :> "autosave" :> "function" :> ReqBody '[JSON] VCObjectHash :> DeleteThrowingVCStoreError '[JSON] ()
    :<|> "delete" :> "scripts" :> Capture "hash" VCObjectHash :> DeleteThrowingVCStoreError '[JSON] ()

vcServer :: (VCHashUpdate a, VCHashUpdate g, FromJSON a, FromJSON g, ToJSON a, ToJSON g, Ord g) => FilePath -> IOTracer VCServerTrace -> RWLock -> Server (VersionControlAPI a g)
vcServer config tracer lock =
  toHandler . fetchFunctionH
    :<|> toHandler . Ops.fetchFunctionsForGroups
    :<|> toHandler . Ops.fetchVCObject
    :<|> toHandler . Ops.fetchVCObjectHistory
    :<|> toHandler . Ops.fetchVCObjects
    :<|> toHandler . Ops.fetchVCObjectClosureHashes
    :<|> toHandler . pushFunctionH
    :<|> toHandler . Ops.deleteAutosavedVCObject
    :<|> toHandler . Ops.deleteVCObjects
  where
    fetchFunctionH h = do
      om@VCMeta {obj} <- Ops.fetchVCObject h
      case obj of
        VCFunction f t -> pure om {obj = (f, t)}
        _ -> Ops.throwError $ Ops.UnexpectedObjectType h $ showVCObjectType obj

    pushFunctionH meta@VCMeta {obj = (f, t)} = Ops.storeVCObject meta {obj = VCFunction f t}

    toHandler :: ReaderT (Ops.VCStorePath, IOTracer VCServerTrace, RWLock) (ExceptT VCServerError Handler) a -> Handler (Union (WithError VCServerError a))
    toHandler = liftTypedError . flip runReaderT (Ops.VCStorePath config, tracer, lock)

runServer :: forall proxy a g. (VCHashUpdate a, VCHashUpdate g, FromJSON a, FromJSON g, ToJSON a, ToJSON g, Ord g) => proxy a -> proxy g -> IO ()
runServer proxyA proxyG = do
  readServerConfig "config.yml" >>= \case
    Left err -> putStrLn err
    Right serverConfig -> runServerConfig proxyA proxyG serverConfig

runServerConfig :: forall proxy a g. (VCHashUpdate a, VCHashUpdate g, FromJSON a, FromJSON g, ToJSON a, ToJSON g, Ord g) => proxy a -> proxy g -> ServerConfig -> IO ()
runServerConfig _ _ serverConfig = do
  let host = fromString . T.unpack . _serverHost $ serverConfig
      port = _serverPort serverConfig
      vcPath = _vcPath serverConfig
      settingsWithTimeout = setTimeout 300 defaultSettings
      tracer = contramap vcServerTraceToString $ IOTracer $ simpleStdOutTracer
      deleteOp = Ops.deleteStaleAutosavedVCObjects :: ReaderT (Ops.VCStorePath, IOTracer VCServerTrace, RWLock) (ExceptT VCServerError IO) ()
      cleanup lock =
        runExceptT (runReaderT deleteOp (Ops.VCStorePath vcPath, tracer, lock)) >>= \case
          Left (VCServerError {serverError}) ->
            traceWith @IOTracer tracer (ThrownVCStoreError serverError)
          Right _ -> pure ()

  runReaderT Ops.initVCStore $ Ops.VCStorePath vcPath
  lock <- RWL.new
  print ("running..." :: String)
  -- Cleanup stale autosave scripts in a separate thread every hour:
  withLinkedAsync_ (forever $ threadDelay 3600000000 >> cleanup lock) $
    -- And run the server:
    runSettings (setPort port $ setHost host settingsWithTimeout) $
      ungzipRequest $
        gzip def $
          serve (Proxy :: Proxy (VersionControlAPI a g)) $
            vcServer vcPath tracer lock

withLinkedAsync_ :: IO a -> IO b -> IO b
withLinkedAsync_ f g = withAsync f $ \h -> link h >> g
