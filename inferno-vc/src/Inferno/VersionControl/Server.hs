{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
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

import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.Thread.Delay (delay)
import Control.Exception (Exception)
import Control.Lens (to, (^.))
import Control.Monad (forM, forever)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Contravariant (contramap)
import Data.Generics.Product (HasField, the)
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Inferno.Types.Syntax (Expr)
import Inferno.Types.Type (TCScheme)
import Inferno.VersionControl.Log (VCServerTrace (ThrownVCOtherError, ThrownVCStoreError), vcServerTraceToText)
import qualified Inferno.VersionControl.Operations as Ops
import qualified Inferno.VersionControl.Operations.Error as Ops
import Inferno.VersionControl.Server.Types (readServerConfig)
import Inferno.VersionControl.Server.UnzipRequest (ungzipRequest)
import Inferno.VersionControl.Types
  ( Pinned,
    VCHashUpdate,
    VCMeta (..),
    VCObject (..),
    VCObjectHash,
    showVCObjectType,
  )
import Network.Wai (Middleware)
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

data VCServerError
  = VCServerError {serverError :: Ops.VCStoreError}
  | VCOtherError {otherError :: T.Text}
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, Exception)

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
  ( VCHashUpdate (Ops.Author m),
    VCHashUpdate (Ops.Group m),
    Ops.InfernoVCOperations VCServerError m,
    Ord (Ops.Group m)
  ) =>
  (forall x. m x -> Handler (Union (WithError VCServerError x))) ->
  Server (VersionControlAPI (Ops.Author m) (Ops.Group m))
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
      Map.fromList <$> forM hs (\h -> (h,) <$> Ops.fetchVCObject h)

runServer ::
  forall m env config.
  ( HasField "serverHost" config config T.Text T.Text,
    HasField "serverPort" config config Int Int,
    VCHashUpdate (Ops.Author m),
    VCHashUpdate (Ops.Group m),
    FromJSON config,
    FromJSON (Ops.Author m),
    FromJSON (Ops.Group m),
    ToJSON (Ops.Author m),
    ToJSON (Ops.Group m),
    Ops.InfernoVCOperations VCServerError m
  ) =>
  (forall x. config -> IOTracer T.Text -> (env -> IO x) -> IO x) ->
  (forall x. m x -> env -> ExceptT VCServerError IO x) ->
  IO ()
runServer withEnv runOp = do
  readServerConfig "config.yml" >>= \case
    Left err -> putStrLn err
    Right serverConfig -> runServerConfig (const id) withEnv runOp serverConfig

runServerConfig ::
  forall m env config.
  ( HasField "serverHost" config config T.Text T.Text,
    HasField "serverPort" config config Int Int,
    VCHashUpdate (Ops.Author m),
    VCHashUpdate (Ops.Group m),
    FromJSON (Ops.Author m),
    FromJSON (Ops.Group m),
    ToJSON (Ops.Author m),
    ToJSON (Ops.Group m),
    Ops.InfernoVCOperations VCServerError m
  ) =>
  (env -> Middleware) ->
  (forall x. config -> IOTracer T.Text -> (env -> IO x) -> IO x) ->
  (forall x. m x -> env -> ExceptT VCServerError IO x) ->
  config ->
  IO ()
runServerConfig middleware withEnv runOp serverConfig = do
  let host = serverConfig ^. the @"serverHost" . to T.unpack . to fromString
      port = serverConfig ^. the @"serverPort"
      settingsWithTimeout = setTimeout 300 defaultSettings

  let tracer = IOTracer (contramap T.unpack simpleStdOutTracer)
      serverTracer = contramap vcServerTraceToText tracer
  withEnv serverConfig tracer $ \env -> do
    let cleanup = do
          -- cutoff is an hour ago
          cutoff <- subtract (60 * 60) <$> liftIO getPOSIXTime
          runExceptT (runOp (Ops.deleteAutosavedVCObjectsOlderThan cutoff) env) >>= \case
            Left (VCServerError {serverError}) ->
              traceWith @IOTracer serverTracer (ThrownVCStoreError serverError)
            Left (VCOtherError {otherError}) ->
              traceWith @IOTracer serverTracer (ThrownVCOtherError otherError)
            Right _ -> pure ()
    print ("running..." :: String)
    -- Cleanup stale autosave scripts in a separate thread every hour:
    withLinkedAsync_ (forever $ delay 3_600_000_000 >> cleanup) $
      -- And run the server:
      runSettings (setPort port $ setHost host settingsWithTimeout) $
        ungzipRequest $
          gzip def $
            middleware env $
              serve (Proxy :: Proxy (VersionControlAPI a g)) $
                vcServer (liftIO . liftTypedError . flip runOp env)

withLinkedAsync_ :: IO a -> IO b -> IO b
withLinkedAsync_ f g = withAsync f $ \h -> link h >> g
