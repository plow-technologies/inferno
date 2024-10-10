{-# LANGUAGE NamedFieldPuns #-}

module Inferno.VersionControl.Client.Cached
  ( VCCacheEnv,
    CachedVCClientError (..),
    fetchVCObjectClosure,
    initVCCachedClient,
  )
where

import Control.Concurrent.STM
  ( TVar,
    atomically,
    newTVarIO,
    readTVar,
    retry,
    writeTVar,
  )
import Control.Monad (forM, forM_)
import Control.Monad.Catch (MonadMask, bracket_)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), asks)
import Crypto.Hash (digestFromByteString)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Either (partitionEithers)
import Data.Generics.Product (HasType, getTyped)
import Data.Generics.Sum (AsType (..))
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
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
import System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix ((</>))

data VCCacheEnv = VCCacheEnv
  { cachePath :: FilePath,
    cacheInFlight :: TVar (Set.Set VCObjectHash)
  }
  deriving (Generic)

-- | Makes sure only one thread at a time fetches the closure for certain
-- VCObjectHashes
withInFlight :: (MonadMask m, MonadIO m) => VCCacheEnv -> [VCObjectHash] -> m a -> m a
withInFlight VCCacheEnv {cacheInFlight} hashes = bracket_ acquire release
  where
    acquire = liftIO $ atomically $ do
      inFlight <- readTVar cacheInFlight
      if any (`Set.member` inFlight) hashes
        then retry
        else do
          writeTVar cacheInFlight $! foldl' (flip Set.insert) inFlight hashes
    release = liftIO $ atomically $ do
      inFlight <- readTVar cacheInFlight
      writeTVar cacheInFlight $! foldl' (flip Set.delete) inFlight hashes

data CachedVCClientError
  = ClientVCStoreError VCServerError
  | ClientServantError ClientError
  | LocalVCStoreError VCStoreError
  deriving (Show, Generic)

initVCCachedClient :: FilePath -> IO VCCacheEnv
initVCCachedClient cachePath = do
  createDirectoryIfMissing True $ cachePath </> "deps"
  cacheInFlight <- newTVarIO mempty
  pure VCCacheEnv {cachePath, cacheInFlight}

fetchVCObjectClosure ::
  ( MonadError err m,
    HasType VCCacheEnv env,
    HasType ClientEnv env,
    AsType VCServerError err,
    AsType ClientError err,
    AsType VCStoreError err,
    MonadReader env m,
    MonadIO m,
    MonadMask m,
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
  env@VCCacheEnv {cachePath} <- asks getTyped
  deps <-
    withInFlight env [objHash] $
      liftIO (doesFileExist $ cachePath </> "deps" </> show objHash) >>= \case
        False -> do
          deps <- liftServantClient $ remoteFetchVCObjectClosureHashes objHash
          liftIO
            $ atomicWriteFile
              (cachePath </> "deps" </> show objHash)
            $ BL.unlines [BL.fromStrict (vcObjectHashToByteString h) | h <- deps]
          pure deps
        True -> fetchVCObjectClosureHashes objHash
  withInFlight env deps $ do
    (nonLocalHashes, localHashes) <-
      partitionEithers
        <$> forM
          deps
          ( \depHash -> do
              liftIO (doesFileExist $ cachePath </> show depHash) >>= \case
                True -> pure $ Right depHash
                False -> pure $ Left depHash
          )
    localObjs <-
      Map.fromList
        <$> forM
          localHashes
          ( \h ->
              (h,) <$> fetchVCObjectUnsafe h
          )

    nonLocalObjs <- liftServantClient $ fetchVCObjects nonLocalHashes
    forM_ (Map.toList nonLocalObjs) $ \(h, o) ->
      liftIO $ atomicWriteFile (cachePath </> show h) $ encode o
    pure $ localObjs `Map.union` nonLocalObjs

fetchVCObjectClosureHashes ::
  ( MonadError err m,
    MonadIO m,
    MonadReader env m,
    AsType VCStoreError err,
    HasType VCCacheEnv env
  ) =>
  VCObjectHash ->
  m [VCObjectHash]
fetchVCObjectClosureHashes h = do
  VCCacheEnv {cachePath} <- asks getTyped
  let fp = cachePath </> "deps" </> show h
  readVCObjectHashTxt fp

readVCObjectHashTxt ::
  ( MonadError err m,
    AsType VCStoreError err,
    MonadIO m
  ) =>
  FilePath ->
  m [VCObjectHash]
readVCObjectHashTxt fp = do
  deps <- filter (not . B.null) . Char8.lines <$> liftIO (B.readFile fp)
  forM deps $ \dep -> do
    decoded <- either (const $ throwing _Typed $ InvalidHash $ Char8.unpack dep) pure $ Base64.decode dep
    maybe (throwing _Typed $ InvalidHash $ Char8.unpack dep) (pure . VCObjectHash) $ digestFromByteString decoded

fetchVCObjectUnsafe ::
  ( MonadReader r m,
    HasType VCCacheEnv r,
    MonadError e m,
    AsType VCStoreError e,
    MonadIO m,
    FromJSON b
  ) =>
  VCObjectHash ->
  m b
fetchVCObjectUnsafe h = do
  VCCacheEnv {cachePath} <- asks getTyped
  let fp = cachePath </> show h
  either (throwing _Typed . CouldNotDecodeObject h) pure =<< liftIO (eitherDecodeStrict <$> Char8.readFile fp)

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
  client <- asks getTyped
  liftIO (runTypedClientM m client) >>= \case
    Left (Left clientErr) -> throwing _Typed clientErr
    Left (Right serverErr) -> throwing _Typed serverErr
    Right res -> pure res
