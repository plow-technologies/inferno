{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Inferno.VersionControl.Operations
-- Description : Operations on the Inferno version control store
--
-- This module defines operations on the Inferno VC store. The store structure is as follows:
--
-- * `<storePath>` stores the JSON serialised `VCMeta VCObject`s, where the filename is the cryptographic hash (`VCOBjectHash`) of the object's contents
-- * `<storePath>/heads` is a set of current HEAD objects of the store, which can be seen as the roots of the VC tree
-- * `<storePath>/to_head` is a map from every `VCOBjectHash` to its current HEAD, where the file name is the source hash and the contents of the file are the HEAD hash
-- * `<storePath>/deps` is a map from every `VCOBjectHash` to its (transitive) dependencies, i.e. the file `<storePath>/deps/<hash>` describes the closure of `<hash>`
module Inferno.VersionControl.Operations where

import Control.Monad (filterM, foldM, forM, forM_)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..))
import Crypto.Hash (digestFromByteString)
import Data.Aeson (FromJSON, ToJSON, Value, eitherDecode, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BL
import Data.Generics.Product (HasType, getTyped)
import Data.Generics.Sum (AsType (..))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Foreign.C.Types (CTime (..))
import GHC.Generics (Generic)
import Inferno.Types.Syntax (Dependencies (..))
import Inferno.VersionControl.Log (VCServerTrace (..))
import Inferno.VersionControl.Operations.Error (VCStoreError (..))
import Inferno.VersionControl.Types
  ( VCHashUpdate,
    VCMeta (..),
    VCObject (..),
    VCObjectHash (..),
    VCObjectPred (..),
    VCObjectVisibility (..),
    vcHash,
    vcObjectHashToByteString,
  )
import Plow.Logging (IOTracer, traceWith)
import System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents, removeFile, renameFile)
import System.FilePath.Posix (takeFileName, (</>))

newtype VCStorePath = VCStorePath FilePath deriving (Generic)

type VCStoreErrM err m = (AsType VCStoreError err, MonadError err m, MonadIO m)

type VCStoreLogM env m = (HasType (IOTracer VCServerTrace) env, MonadReader env m, MonadIO m)

type VCStoreEnvM env m = (HasType VCStorePath env, MonadReader env m, MonadIO m)

trace :: VCStoreLogM env m => VCServerTrace -> m ()
trace t = do
  tracer <- getTyped <$> ask
  traceWith @IOTracer tracer t

throwError :: (VCStoreLogM env m, VCStoreErrM err m) => VCStoreError -> m a
throwError e = do
  trace $ ThrownVCStoreError e
  throwing _Typed e

initVCStore :: VCStoreEnvM env m => m ()
initVCStore =
  (getTyped <$> ask) >>= \(VCStorePath storePath) -> liftIO $ do
    createDirectoryIfMissing True $ storePath </> "heads"
    createDirectoryIfMissing True $ storePath </> "to_head"
    createDirectoryIfMissing True $ storePath </> "deps"

initVCCachedClient :: VCStoreEnvM env m => m ()
initVCCachedClient =
  (getTyped <$> ask) >>= \(VCStorePath storePath) -> liftIO $ do
    createDirectoryIfMissing True $ storePath </> "deps"

checkPathExists :: (VCStoreLogM env m, VCStoreErrM err m) => FilePath -> m ()
checkPathExists fp =
  liftIO (doesFileExist fp) >>= \case
    False -> throwError $ CouldNotFindPath fp
    True -> pure ()

getDepsFromStore :: (VCStoreLogM env m, VCStoreErrM err m) => FilePath -> VCObjectHash -> m BL.ByteString
getDepsFromStore path h = do
  let fp = path </> show h
  checkPathExists fp
  liftIO $ BL.readFile $ path </> show h

appendBS :: (VCStoreLogM env m) => FilePath -> BL.ByteString -> m ()
appendBS fp bs = do
  trace $ WriteTxt fp
  liftIO $ BL.appendFile fp bs

writeBS :: (VCStoreLogM env m) => FilePath -> BL.ByteString -> m ()
writeBS fp bs = do
  trace $ WriteTxt fp
  liftIO $ BL.writeFile fp bs

writeHashedJSON :: (VCStoreLogM env m, VCHashUpdate obj, ToJSON obj) => FilePath -> obj -> m VCObjectHash
writeHashedJSON path o = do
  let h = vcHash o
  exists <- liftIO $ doesFileExist path
  if exists
    then trace $ AlreadyExistsJSON (path </> show h)
    else do
      trace $ WriteJSON (path </> show h)
      liftIO $ BL.writeFile (path </> show h) $ encode o
  pure h

readVCObjectHashTxt :: (VCStoreLogM env m, VCStoreErrM err m) => FilePath -> m [VCObjectHash]
readVCObjectHashTxt fp = do
  checkPathExists fp
  trace $ ReadTxt fp
  deps <- filter (not . B.null) . Char8.lines <$> (liftIO $ B.readFile fp)
  forM deps $ \dep -> do
    decoded <- either (const $ throwError $ InvalidHash $ Char8.unpack dep) pure $ Base64.decode dep
    maybe (throwError $ InvalidHash $ Char8.unpack dep) (pure . VCObjectHash) $ digestFromByteString decoded

storeVCObject :: (VCStoreLogM env m, VCStoreErrM err m, VCStoreEnvM env m, VCHashUpdate a, VCHashUpdate g, ToJSON a, ToJSON g) => VCMeta a g VCObject -> m VCObjectHash
storeVCObject obj@VCMeta {obj = ast, pred = p} = do
  VCStorePath storePath <- getTyped <$> ask
  -- if the new object has a direct predecessor (i.e. is not a clone or an initial commit)
  --  we need to make sure that the predecessor is currently a HEAD object in the store
  let maybeCurrentHead = case p of
        Init -> Nothing
        CloneOf _ -> Nothing
        CloneOfRemoved _ -> Nothing
        CloneOfNotFound _ -> Nothing
        CompatibleWithPred h -> Just h
        IncompatibleWithPred h _ -> Just h
        MarkedBreakingWithPred h -> Just h

  obj_h <- case maybeCurrentHead of
    Just pred_hash -> do
      let head_fp = storePath </> "heads" </> show pred_hash
      -- check to see if pred_hash exists in '<storePath>/heads`
      exists_head <- liftIO $ doesFileExist head_fp
      if exists_head
        then do
          -- we know that pred_h is currently HEAD, we can therefore store the object and metadata in the store
          obj_h <- writeHashedJSON storePath obj
          -- next we make the newly added object the HEAD
          let new_head_fp = storePath </> "heads" </> show obj_h
          liftIO $ renameFile head_fp new_head_fp
          -- we append the previous head hash to the file (this serves as lookup for all the predecessors)
          appendBS new_head_fp $ BL.fromStrict $ vcObjectHashToByteString pred_hash <> "\n"
          -- now we need to change all the predecessor mappings in '<storePath>/to_head' to point to the new HEAD
          -- we also include the new head pointing to itself
          preds <- readVCObjectHashTxt new_head_fp
          let obj_h_bs = BL.fromStrict $ vcObjectHashToByteString obj_h
          forM_ (obj_h : preds) $ \pred_h ->
            writeBS (storePath </> "to_head" </> show pred_h) obj_h_bs

          pure obj_h
        else throwError $ TryingToAppendToNonHead pred_hash
    Nothing -> do
      obj_h <- writeHashedJSON storePath obj
      -- as there is no previous HEAD for this object, we simply create a new one
      let new_head_fp = storePath </> "heads" </> show obj_h
      appendBS new_head_fp $ case p of
        -- in case this is a clone of another object, we add its hash to the history
        CloneOf clone_h -> BL.fromStrict $ vcObjectHashToByteString clone_h <> "\n"
        _ -> mempty

      -- we again make sure to add a self reference link to the '<storePath>/to_head' map
      let obj_h_bs = BL.fromStrict $ vcObjectHashToByteString obj_h
      writeBS (storePath </> "to_head" </> show obj_h) obj_h_bs
      pure obj_h

  -- finally, we store the dependencies of the commited object by fetching the dependencies from the AST
  let deps = Set.toList $ getDependencies ast
  writeBS (storePath </> "deps" </> show obj_h) mempty
  forM_ deps $ \dep_h -> do
    -- first we append the direct dependency hash 'dep_h'
    appendBS (storePath </> "deps" </> show obj_h) $ BL.fromStrict $ vcObjectHashToByteString dep_h <> "\n"
    -- then we append the transitive dependencies of the given object, pointed to by the hash 'dep_h'
    appendBS (storePath </> "deps" </> show obj_h) =<< getDepsFromStore (storePath </> "deps") dep_h

  pure obj_h

-- | Delete a temporary object from the VC. This is used for autosaved scripts
-- and to run tests against unsaved scripts
deleteAutosavedVCObject :: (VCStoreLogM env m, VCStoreErrM err m, VCStoreEnvM env m) => VCObjectHash -> m ()
deleteAutosavedVCObject obj_hash = do
  VCStorePath storePath <- getTyped <$> ask
  -- check if object meta exists with hash meta_hash, and get meta
  (VCMeta {name = obj_name} :: VCMeta Value Value VCObject) <- fetchVCObject obj_hash
  -- check that it is safe to delete
  if obj_name == pack "<AUTOSAVE>"
    then do
      -- delete object, object meta, head/to_head, and deps
      deleteFile $ (storePath </> show obj_hash)
      deleteFile $ (storePath </> "heads" </> show obj_hash)
      deleteFile $ (storePath </> "to_head" </> show obj_hash)
      deleteFile $ (storePath </> "deps" </> show obj_hash)
    else throwError $ TryingToDeleteNonAutosave obj_name
  where
    deleteFile fp = do
      trace $ DeleteFile fp
      liftIO $ removeFile fp

-- | Deletes all stale autosaved objects from the VC.
deleteStaleAutosavedVCObjects :: (VCStoreLogM env m, VCStoreErrM err m, VCStoreEnvM env m) => m ()
deleteStaleAutosavedVCObjects = do
  -- We know that all autosaves must be heads:
  heads <- getAllHeads
  forM_
    heads
    ( \h -> do
        -- fetch object, check name and timestamp
        (VCMeta {name, timestamp} :: VCMeta Value Value VCObject) <- fetchVCObject h
        now <- liftIO $ CTime . round . toRational <$> getPOSIXTime
        if name == pack "<AUTOSAVE>" && timestamp < now - 60 * 60
          then -- delete the stale ones (> 1hr old)
            deleteAutosavedVCObject h
          else pure ()
    )

-- | Soft delete script and its predecessors
-- All scripts and their references are moved to "removed" directory
deleteVCObjects :: (VCStoreLogM env m, VCStoreErrM err m, VCStoreEnvM env m) => VCObjectHash -> m ()
deleteVCObjects obj_hash = do
  VCStorePath storePath <- getTyped <$> ask
  liftIO $ do
    createDirectoryIfMissing True $ storePath </> "removed"
    createDirectoryIfMissing True $ storePath </> "removed" </> "heads"
    createDirectoryIfMissing True $ storePath </> "removed" </> "to_head"
    createDirectoryIfMissing True $ storePath </> "removed" </> "deps"

  (metas :: [VCMeta Value Value VCObjectHash]) <- fetchVCObjectHistory obj_hash
  forM_ metas $ \VCMeta {obj = hash} -> do
    forM_
      [ show hash,
        "heads" </> show hash,
        "to_head" </> show hash,
        "deps" </> show hash
      ]
      $ \source_fp -> safeRenameFile (storePath </> source_fp) (storePath </> "removed" </> source_fp)
  where
    safeRenameFile source target = do
      liftIO (doesFileExist source) >>= \case
        False -> pure ()
        True -> liftIO $ renameFile source target

fetchVCObject :: (VCStoreLogM env m, VCStoreErrM err m, VCStoreEnvM env m, FromJSON a, FromJSON g) => VCObjectHash -> m (VCMeta a g VCObject)
fetchVCObject = fetchVCObject' Nothing

-- | Fetch object from removed directory
fetchRemovedVCObject :: (VCStoreLogM env m, VCStoreErrM err m, VCStoreEnvM env m, FromJSON a, FromJSON g) => VCObjectHash -> m (VCMeta a g VCObject)
fetchRemovedVCObject = fetchVCObject' (Just "removed")

fetchVCObject' :: (VCStoreLogM env m, VCStoreErrM err m, VCStoreEnvM env m, FromJSON a, FromJSON g) => Maybe FilePath -> VCObjectHash -> m (VCMeta a g VCObject)
fetchVCObject' mprefix h = do
  VCStorePath storePath <- getTyped <$> ask
  let fp = case mprefix of
        Nothing -> storePath </> show h
        Just prefix -> storePath </> prefix </> show h
  checkPathExists fp
  trace $ ReadJSON fp
  either (throwError . CouldNotDecodeObject h) pure =<< (liftIO $ eitherDecode <$> BL.readFile fp)

fetchVCObjects :: (VCStoreLogM env m, VCStoreErrM err m, VCStoreEnvM env m, FromJSON a, FromJSON g) => [VCObjectHash] -> m (Map.Map VCObjectHash (VCMeta a g VCObject))
fetchVCObjects hs = do
  Map.fromList <$> (forM hs $ \h -> (h,) <$> fetchVCObject h)

fetchVCObjectClosureHashes :: (VCStoreLogM env m, VCStoreErrM err m, VCStoreEnvM env m) => VCObjectHash -> m [VCObjectHash]
fetchVCObjectClosureHashes h = do
  VCStorePath storePath <- getTyped <$> ask
  let fp = storePath </> "deps" </> show h
  readVCObjectHashTxt fp

fetchVCObjectWithClosure :: (VCStoreLogM env m, VCStoreErrM err m, VCStoreEnvM env m, FromJSON a, FromJSON g) => VCObjectHash -> m (Map.Map VCObjectHash (VCMeta a g VCObject))
fetchVCObjectWithClosure h = do
  deps <- fetchVCObjectClosureHashes h
  Map.fromList <$> (forM deps $ \dep -> (dep,) <$> fetchVCObject dep)

calculateMissingVCObjects :: VCStoreEnvM env m => [VCObjectHash] -> m [VCObjectHash]
calculateMissingVCObjects = filterM $ \h -> do
  VCStorePath storePath <- getTyped <$> ask
  not <$> (liftIO $ doesFileExist $ storePath </> show h)

fetchCurrentHead :: (VCStoreLogM env m, VCStoreErrM err m, VCStoreEnvM env m) => VCObjectHash -> m VCObjectHash
fetchCurrentHead h = do
  VCStorePath storePath <- getTyped <$> ask
  let fp = storePath </> "to_head" </> show h
  exists <- liftIO $ doesFileExist fp
  if exists
    then
      readVCObjectHashTxt fp >>= \case
        [h'] -> pure h'
        _ -> throwError $ CouldNotFindHead h
    else throwError $ CouldNotFindHead h

fetchVCObjectHistory :: (VCStoreLogM env m, VCStoreErrM err m, VCStoreEnvM env m, FromJSON a, FromJSON g) => VCObjectHash -> m [VCMeta a g VCObjectHash]
fetchVCObjectHistory h = do
  head_h <- fetchCurrentHead h
  VCStorePath storePath <- getTyped <$> ask
  let head_fp = storePath </> "heads" </> show head_h
  preds <- readVCObjectHashTxt head_fp
  -- When we fold the preds, we check if they exist in two places
  -- 1. in 'vc_store' for available scripts
  -- 2. then in 'vc_store/removed' for scripts that have been deleted
  -- If a script has been deleted, we track its hash.
  let f acc hsh = do
        existsInRoot <- liftIO $ doesFileExist $ storePath </> show hsh
        existsInRemoved <- liftIO $ doesFileExist $ storePath </> "removed" </> show hsh

        if existsInRoot
          then do
            obj <- fmap (const hsh) <$> fetchVCObject hsh
            pure ((obj : fst acc), snd acc)
          else do
            if existsInRemoved
              then do
                obj <- fmap (const hsh) <$> fetchRemovedVCObject hsh
                pure ((obj : fst acc), (hsh : snd acc))
              else -- This script no longer exists even in 'removed' directory. The directory might get cleaned up by accident or something.
              -- There are two choices we can make,
              -- 1. Return a `VCMeta VCObjectHash` with dummy data
              -- 2. Ignore this meta.
              -- Approach no. 2 is taken here by just returning the accumulator.
                pure acc
  (metas, removeds) <- foldM f ([], []) (head_h : preds)
  -- We like to know if the source of the clone still exists. We can do this by checking against the deleted hashes
  -- that we tracked above.
  pure $ case removeds of
    [] -> metas
    _ ->
      fmap
        ( \meta -> case Inferno.VersionControl.Types.pred meta of
            CloneOf hsh'
              | List.elem hsh' removeds ->
                  -- The source of the clone script has been deleted, so we alter its 'pred' field as 'CloneOfRemoved' but
                  -- with the same hash. This way the upstream system (e.g. onping/frontend) can differentiate between
                  -- source that is still available and no longer available.
                  -- This does not change the way the script is persisted in the db, it is still stored as 'CloneOf'.
                  -- See 'CloneOfRemoved' for details.
                  meta {Inferno.VersionControl.Types.pred = CloneOfRemoved hsh'}
            _ -> meta
        )
        metas

getAllHeads :: (VCStoreLogM env m, VCStoreEnvM env m) => m [VCObjectHash]
getAllHeads = do
  VCStorePath storePath <- getTyped <$> ask
  headsRaw <- liftIO $ getDirectoryContents $ storePath </> "heads"
  pure $
    foldr
      ( \hd xs ->
          case (either (const $ Nothing) Just $ Base64.decode $ Char8.pack hd) >>= digestFromByteString of
            Just hsh -> (VCObjectHash hsh) : xs
            Nothing -> xs
      )
      []
      (map takeFileName headsRaw)

fetchFunctionsForGroups :: (VCStoreLogM env m, VCStoreErrM err m, VCStoreEnvM env m, Ord g, FromJSON a, FromJSON g) => Set.Set g -> m [VCMeta a g VCObjectHash]
fetchFunctionsForGroups grps = do
  heads <- getAllHeads
  foldM
    ( \objs hsh -> do
        meta@VCMeta {obj, visibility, group} <- fetchVCObject hsh
        pure $ case obj of
          VCFunction _ _ ->
            if visibility == VCObjectPublic || group `Set.member` grps
              then (fmap (const hsh) meta) : objs
              else objs
          _ -> objs
    )
    []
    heads
