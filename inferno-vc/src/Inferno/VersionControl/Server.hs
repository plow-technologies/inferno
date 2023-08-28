{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Inferno.VersionControl.Server
  ( VCServerError (..),
    VersionControlAPI,
    runServer,
    runServerConfig,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (link, withAsync)
import Control.Monad (forM, forever)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
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

vcServer ::
  ( VCHashUpdate a,
    VCHashUpdate g,
    Ops.InfernoVCOperations VCServerError m,
    Ops.Deserializable m a,
    Ops.Deserializable m g,
    Ops.Serializable m a,
    Ops.Serializable m g,
    Ord g
  ) =>
  (forall x. m x -> Handler (Union (WithError VCServerError x))) ->
  Server (VersionControlAPI a g)
vcServer toHandler =
  toHandler . fetchFunctionH
    :<|> toHandler . Ops.fetchFunctionsForGroups
    :<|> toHandler . Ops.fetchVCObject
    :<|> toHandler . Ops.fetchVCObjectHistory
    :<|> toHandler . fetchVCObjects
    :<|> toHandler . Ops.fetchVCObjectClosureHashes
    :<|> toHandler . pushFunctionH
    :<|> toHandler . Ops.deleteAutosavedVCObject
    :<|> toHandler . Ops.deleteVCObjects
  where
    fetchFunctionH h = do
      om@VCMeta {obj} <- Ops.fetchVCObject h
      case obj of
        VCFunction f t -> pure om {obj = (f, t)}
        _ -> throwError $ VCServerError $ Ops.UnexpectedObjectType h $ showVCObjectType obj

    pushFunctionH meta@VCMeta {obj = (f, t)} = Ops.storeVCObject meta {obj = VCFunction f t}

    fetchVCObjects hs =
      Map.fromList <$> (forM hs $ \h -> (h,) <$> Ops.fetchVCObject h)

runServer ::
  forall proxy m a g env.
  ( VCHashUpdate a,
    VCHashUpdate g,
    FromJSON a,
    FromJSON g,
    ToJSON a,
    ToJSON g,
    Ops.Deserializable m a,
    Ops.Deserializable m g,
    Ops.Serializable m a,
    Ops.Serializable m g,
    Ops.InfernoVCOperations VCServerError m,
    Ord g
  ) =>
  proxy a ->
  proxy g ->
  (FilePath -> IOTracer VCServerTrace -> IO env) ->
  (forall x. m x -> env -> ExceptT VCServerError IO x) ->
  IO ()
runServer proxyA proxyG initEnv runOp = do
  readServerConfig "config.yml" >>= \case
    Left err -> putStrLn err
    Right serverConfig -> runServerConfig proxyA proxyG initEnv runOp serverConfig

runServerConfig ::
  forall proxy m a g env.
  ( VCHashUpdate a,
    VCHashUpdate g,
    FromJSON a,
    FromJSON g,
    ToJSON a,
    ToJSON g,
    Ops.Deserializable m a,
    Ops.Deserializable m g,
    Ops.Serializable m a,
    Ops.Serializable m g,
    Ops.InfernoVCOperations VCServerError m,
    Ord g
  ) =>
  proxy a ->
  proxy g ->
  (FilePath -> IOTracer VCServerTrace -> IO env) ->
  (forall x. m x -> env -> ExceptT VCServerError IO x) ->
  ServerConfig ->
  IO ()
runServerConfig _ _ initEnv runOp serverConfig = do
  let host = fromString . T.unpack . _serverHost $ serverConfig
      port = _serverPort serverConfig
      vcPath = _vcPath serverConfig
      settingsWithTimeout = setTimeout 300 defaultSettings
      tracer = contramap vcServerTraceToString $ IOTracer simpleStdOutTracer

  env <- initEnv vcPath tracer
  let cleanup =
        runExceptT (runOp Ops.deleteStaleAutosavedVCObjects env) >>= \case
          Left (VCServerError {serverError}) ->
            traceWith @IOTracer tracer (ThrownVCStoreError serverError)
          Right _ -> pure ()
  print ("running..." :: String)
  -- Cleanup stale autosave scripts in a separate thread every hour:
  withLinkedAsync_ (forever $ threadDelay 3600000000 >> cleanup) $
    -- And run the server:
    runSettings (setPort port $ setHost host settingsWithTimeout) $
      ungzipRequest $
        gzip def $
          serve (Proxy :: Proxy (VersionControlAPI a g)) $
            vcServer (liftIO . liftTypedError . flip runOp env)

withLinkedAsync_ :: IO a -> IO b -> IO b
withLinkedAsync_ f g = withAsync f $ \h -> link h >> g
