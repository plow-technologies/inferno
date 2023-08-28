module Inferno.VersionControl.Client.Cached
  ( VCCachePath (..),
    CachedVCClientError (..),
    fetchVCObjectClosure,
    initVCCachedClient,
  )
where

import Control.Monad (forM, forM_)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), asks)
import Crypto.Hash (digestFromByteString)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BL
import System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import Data.Either (partitionEithers)
import Data.Generics.Product (HasType, getTyped)
import Data.Generics.Sum (AsType (..))
import qualified Data.Map as Map
import GHC.Generics (Generic)
import qualified Inferno.VersionControl.Client as VCClient
import Inferno.VersionControl.Operations.Error (VCStoreError (..))
import Inferno.VersionControl.Server (VCServerError)
import Inferno.VersionControl.Types
  ( VCMeta,
    VCObject,
    VCObjectHash (..),
    vcObjectHashToByteString,
  )
import Servant.Client (ClientEnv, ClientError)
import Servant.Typed.Error (TypedClientM, runTypedClientM)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix ((</>))

newtype VCCachePath = VCCachePath FilePath deriving (Generic)

data CachedVCClientError
  = ClientVCStoreError VCServerError
  | ClientServantError ClientError
  | LocalVCStoreError VCStoreError
  deriving (Show, Generic)

initVCCachedClient :: VCCachePath -> IO ()
initVCCachedClient (VCCachePath storePath) =
  createDirectoryIfMissing True $ storePath </> "deps"


fetchVCObjectClosure ::
  ( MonadError err m,
    HasType VCCachePath env,
    HasType ClientEnv env,
    AsType VCServerError err,
    AsType ClientError err,
    AsType VCStoreError err,
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
fetchVCObjectClosure fetchVCObjects remoteFetchVCObjectClosureHashes objHash = do
  VCCachePath storePath <- getTyped <$> ask
  deps <-
    (liftIO $ doesFileExist $ storePath </> "deps" </> show objHash) >>= \case
      False -> do
        deps <- liftServantClient $ remoteFetchVCObjectClosureHashes objHash
        liftIO
          $ atomicWriteFile
            (storePath </> "deps" </> show objHash)
          $ BL.concat [BL.fromStrict (vcObjectHashToByteString h) <> "\n" | h <- deps]
        pure deps
      True -> fetchVCObjectClosureHashes objHash
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
              (h,) <$> fetchVCObjectUnsafe h
          )

  nonLocalObjs <- liftServantClient $ fetchVCObjects nonLocalHashes
  forM_ (Map.toList nonLocalObjs) $ \(h, o) ->
    liftIO $ atomicWriteFile (storePath </> show h) $ encode o
  pure $ localObjs `Map.union` nonLocalObjs

fetchVCObjectClosureHashes ::
  ( MonadError err m,
    MonadIO m,
    MonadReader env m,
    AsType VCStoreError err,
    HasType VCCachePath env
  ) =>
  VCObjectHash ->
  m [VCObjectHash]
fetchVCObjectClosureHashes h = do
  VCCachePath storePath <- asks getTyped
  let fp = storePath </> "deps" </> show h
  readVCObjectHashTxt fp

readVCObjectHashTxt ::
  ( MonadError err m,
    AsType VCStoreError err,
    MonadIO m
  ) =>
  FilePath ->
  m [VCObjectHash]
readVCObjectHashTxt fp = do
  deps <- filter (not . B.null) . Char8.lines <$> (liftIO $ B.readFile fp)
  forM deps $ \dep -> do
    decoded <- either (const $ throwing _Typed $ InvalidHash $ Char8.unpack dep) pure $ Base64.decode dep
    maybe (throwing _Typed $ InvalidHash $ Char8.unpack dep) (pure . VCObjectHash) $ digestFromByteString decoded

fetchVCObjectUnsafe ::
  ( MonadReader r m,
    HasType VCCachePath r,
    MonadError e m,
    AsType VCStoreError e,
    MonadIO m,
    FromJSON b
  ) =>
  VCObjectHash ->
  m b
fetchVCObjectUnsafe h = do
  VCCachePath storePath <- asks getTyped
  let fp = storePath </> show h
  either (throwing _Typed . CouldNotDecodeObject h) pure =<< liftIO (eitherDecode <$> BL.readFile fp)

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
    Left (Left clientErr) -> throwing _Typed clientErr
    Left (Right serverErr) -> throwing _Typed serverErr
    Right res -> pure res
