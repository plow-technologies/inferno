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
import Control.Monad (forM, forM_, guard)
import Control.Monad.Catch (MonadMask, bracket_, tryJust)
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
import Data.Generics.Product (HasType, getTyped)
import Data.Generics.Sum (AsType (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import qualified Inferno.VersionControl.Client as VCClient
import Inferno.VersionControl.Log (VCCacheTrace (..))
import Inferno.VersionControl.Operations.Error (VCStoreError (..))
import Inferno.VersionControl.Server (VCServerError)
import Inferno.VersionControl.Types
  ( VCMeta,
    VCObject,
    VCObjectHash (..),
    vcObjectHashToByteString,
  )
import Plow.Logging (IOTracer (..), traceWith)
import Servant.Client (ClientEnv, ClientError)
import Servant.Typed.Error (TypedClientM, runTypedClientM)
import System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>))
import System.IO.Error (isDoesNotExistError)
import System.Random.Shuffle (shuffleM)

data VCCacheEnv = VCCacheEnv
  { cachePath :: FilePath,
    cacheObjInFlight :: TVar (Set.Set VCObjectHash),
    cacheDepInFlight :: TVar (Set.Set VCObjectHash),
    tracer :: IOTracer VCCacheTrace
  }
  deriving (Generic)

-- | Prevents thundering-herd problem by locking the key 'k' so only a single
-- 'fetchAndSave' runs concurrently
withSingleConcurrentFetch ::
  (MonadMask m, MonadIO m, Ord k) =>
  -- | A 'TVar' holding the set of keys which are currently being fetched
  TVar (Set.Set k) ->
  -- | A function that returns Just the value being cached if found locally or
  -- Nothing if it hasn't being cached yet. No lock is taken when calling this
  -- function
  (k -> m (Maybe a)) ->
  -- | A function that fetches the value and caches it. A lock is taken so only
  -- a single call per key to this function runs concurrently
  (k -> m a) ->
  -- | The key associated to the value being cached
  k ->
  m a
withSingleConcurrentFetch keySetRef check fetchAndSave key =
  check key >>= \case
    Just x -> pure x
    Nothing ->
      bracket_ acquire release $
        -- check again because another thread may have fetched while we were
        -- blocked
        check key >>= \case
          Just x -> pure x
          Nothing -> fetchAndSave key
  where
    acquire = liftIO $ atomically $ do
      inFlight <- readTVar keySetRef
      if key `Set.member` inFlight
        then retry
        else do
          writeTVar keySetRef $! key `Set.insert` inFlight
    release = liftIO $ atomically $ do
      inFlight <- readTVar keySetRef
      writeTVar keySetRef $! key `Set.delete` inFlight

data CachedVCClientError
  = ClientVCStoreError VCServerError
  | ClientServantError ClientError
  | LocalVCStoreError VCStoreError
  deriving (Show, Generic)

initVCCachedClient :: FilePath -> IOTracer VCCacheTrace -> IO VCCacheEnv
initVCCachedClient cachePath tracer = do
  createDirectoryIfMissing True $ cachePath </> "deps"
  cacheObjInFlight <- newTVarIO mempty
  cacheDepInFlight <- newTVarIO mempty
  pure VCCacheEnv {cachePath, cacheObjInFlight, cacheDepInFlight, tracer}

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
  VCCacheEnv {cacheObjInFlight, cacheDepInFlight} <- asks getTyped
  deps <- withSingleConcurrentFetch cacheDepInFlight maybeReadCachedClosureHashes (fetchAndCacheClosureHashes remoteFetchVCObjectClosureHashes) objHash
  -- shuffle scriptIds to improve concurrent performance when cache is cold
  shuffledDeps <- liftIO $ shuffleM $ objHash : deps
  mconcat
    <$> mapM (withSingleConcurrentFetch cacheObjInFlight maybeReadCachedVCObject (fetchAndCacheVCObject fetchVCObjects)) shuffledDeps

maybeReadCachedClosureHashes ::
  ( MonadError err m,
    HasType VCCacheEnv env,
    AsType VCStoreError err,
    MonadReader env m,
    MonadIO m,
    MonadMask m
  ) =>
  VCObjectHash ->
  m (Maybe [VCObjectHash])
maybeReadCachedClosureHashes objHash = do
  VCCacheEnv {tracer} <- asks getTyped
  tryJust (guard . isDoesNotExistError) readCachedClosureHashes >>= \case
    Right deps ->
      Just deps <$ traceWith tracer (VCCacheDepsHit objHash)
    Left _ ->
      Nothing <$ traceWith tracer (VCCacheDepsMiss objHash)
  where
    readCachedClosureHashes = do
      path <- cachedDepsPath objHash
      deps <- filter (not . B.null) . Char8.lines <$> liftIO (B.readFile path)
      forM deps $ \dep -> do
        decoded <-
          either (const $ throwing _Typed $ InvalidHash $ Char8.unpack dep) pure $
            Base64.decode dep
        maybe (throwing _Typed $ InvalidHash $ Char8.unpack dep) (pure . VCObjectHash) $
          digestFromByteString decoded

fetchAndCacheClosureHashes ::
  ( MonadError err m,
    HasType VCCacheEnv env,
    HasType ClientEnv env,
    AsType VCServerError err,
    AsType ClientError err,
    MonadReader env m,
    MonadIO m
  ) =>
  (VCObjectHash -> VCClient.ClientMWithVCStoreError [VCObjectHash]) ->
  VCObjectHash ->
  m [VCObjectHash]
fetchAndCacheClosureHashes remoteFetchVCObjectClosureHashes objHash = do
  deps <- liftServantClient $ remoteFetchVCObjectClosureHashes objHash
  path <- cachedDepsPath objHash
  liftIO $
    atomicWriteFile path $
      BL.unlines $
        map (BL.fromStrict . vcObjectHashToByteString) deps
  pure deps

maybeReadCachedVCObject ::
  ( MonadReader r m,
    HasType VCCacheEnv r,
    MonadError e m,
    AsType VCStoreError e,
    MonadIO m,
    MonadMask m,
    FromJSON b
  ) =>
  VCObjectHash ->
  m (Maybe (Map.Map VCObjectHash b))
maybeReadCachedVCObject objHash = do
  VCCacheEnv {tracer} <- asks getTyped
  tryJust (guard . isDoesNotExistError) readCachedVCObject >>= \case
    Left _ ->
      Nothing <$ traceWith tracer (VCCacheMiss objHash)
    Right obj ->
      Just (Map.singleton objHash obj) <$ traceWith tracer (VCCacheHit objHash)
  where
    readCachedVCObject = do
      path <- cachedObjPath objHash
      either (throwing _Typed . CouldNotDecodeObject objHash) pure
        =<< liftIO (eitherDecodeStrict <$> Char8.readFile path)

fetchAndCacheVCObject ::
  ( MonadError err m,
    HasType VCCacheEnv env,
    HasType ClientEnv env,
    AsType VCServerError err,
    AsType ClientError err,
    MonadReader env m,
    MonadIO m,
    ToJSON a,
    ToJSON g
  ) =>
  ([VCObjectHash] -> VCClient.ClientMWithVCStoreError (Map.Map VCObjectHash (VCMeta a g VCObject))) ->
  VCObjectHash ->
  m (Map.Map VCObjectHash (VCMeta a g VCObject))
fetchAndCacheVCObject fetchVCObjects objHash = do
  objs <- liftServantClient $ fetchVCObjects [objHash]
  forM_ (Map.toList objs) $ \(h, o) -> do
    path <- cachedObjPath h
    liftIO $ atomicWriteFile path $ encode o
  pure objs

cachedDepsPath :: (MonadReader r m, HasType VCCacheEnv r) => VCObjectHash -> m FilePath
cachedDepsPath objHash = do
  VCCacheEnv {cachePath} <- asks getTyped
  pure $ cachePath </> "deps" </> show objHash

cachedObjPath :: (MonadReader r m, HasType VCCacheEnv r) => VCObjectHash -> m FilePath
cachedObjPath objHash = do
  VCCacheEnv {cachePath} <- asks getTyped
  pure $ cachePath </> show objHash

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
