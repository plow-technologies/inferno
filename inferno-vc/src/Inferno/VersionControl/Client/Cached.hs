module Inferno.VersionControl.Client.Cached where

import Control.Monad (forM, forM_)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..))
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Either (partitionEithers)
import Data.Generics.Product (HasType, getTyped)
import Data.Generics.Sum (AsType (..))
import qualified Data.Map as Map
import GHC.Generics (Generic)
import qualified Inferno.VersionControl.Client as VCClient
import Inferno.VersionControl.Log (VCServerTrace)
import qualified Inferno.VersionControl.Operations as Ops
import qualified Inferno.VersionControl.Operations.Error as Ops
import Inferno.VersionControl.Server (VCServerError)
import Inferno.VersionControl.Types
  ( VCMeta,
    VCObject,
    VCObjectHash,
    vcObjectHashToByteString,
  )
import Plow.Logging (IOTracer)
import Servant.Client (ClientEnv, ClientError)
import Servant.Typed.Error (TypedClientM, runTypedClientM)
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>))

data CachedVCClientError
  = ClientVCStoreError VCServerError
  | ClientServantError ClientError
  | LocalVCStoreError Ops.VCStoreError
  deriving (Show, Generic)

liftServantClient ::
  ( MonadError e m,
    MonadIO m,
    MonadReader s m,
    HasType ClientEnv s,
    AsType a e,
    AsType ClientError e
  ) =>
  TypedClientM a b ->
  m b
liftServantClient m = do
  client <- getTyped <$> ask
  (liftIO $ runTypedClientM m client) >>= \case
    Left (Left clientErr) -> throwing _Typed $ clientErr
    Left (Right serverErr) -> throwing _Typed $ serverErr
    Right res -> pure res

fetchVCObjectClosure ::
  ( AsType VCServerError err,
    AsType ClientError err,
    AsType Ops.VCStoreError err,
    MonadError err m,
    HasType (IOTracer VCServerTrace) env,
    HasType Ops.VCStorePath env,
    HasType ClientEnv env,
    MonadReader env m,
    MonadIO m,
    FromJSON a,
    FromJSON g,
    ToJSON a,
    ToJSON g
  ) =>
  ([VCObjectHash] -> VCClient.ClientMWithVCStoreError (Map.Map VCObjectHash (VCMeta a g VCObject))) ->
  (VCObjectHash -> VCClient.ClientMWithVCStoreError [VCObjectHash]) ->
  VCObjectHash ->
  m (Map.Map VCObjectHash (VCMeta a g VCObject))
fetchVCObjectClosure fetchVCObjects fetchVCObjectClosureHashes objHash = do
  Ops.VCStorePath storePath <- getTyped <$> ask
  deps <-
    (liftIO $ doesFileExist $ storePath </> "deps" </> show objHash) >>= \case
      False -> do
        deps <- liftServantClient $ fetchVCObjectClosureHashes objHash
        Ops.writeBS
          (storePath </> "deps" </> show objHash)
          $ BL.concat [BL.fromStrict (vcObjectHashToByteString h) <> "\n" | h <- deps]
        pure deps
      True -> Ops.fetchVCObjectClosureHashes objHash
  (nonLocalHashes, localHashes) <-
    partitionEithers
      <$> ( forM (objHash : deps) $ \depHash -> do
              (liftIO $ doesFileExist $ storePath </> show depHash) >>= \case
                True -> pure $ Right depHash
                False -> pure $ Left depHash
          )
  localObjs <-
    Map.fromList
      <$> ( forM localHashes $ \h ->
              (h,) <$> Ops.fetchVCObject h
          )

  nonLocalObjs <- liftServantClient $ fetchVCObjects nonLocalHashes
  forM_ (Map.toList nonLocalObjs) $ \(h, o) ->
    liftIO $ BL.writeFile (storePath </> show h) $ encode o
  pure $ localObjs `Map.union` nonLocalObjs
