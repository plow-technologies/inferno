{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Inferno.VersionControl.Operations.Filesystem
-- Description : InfernoVCOperations implementation that uses the filesystem
--
-- This module defines operations on the Inferno VC store. The store structure is as follows:
--
-- * `<storePath>` stores the JSON serialised `VCMeta VCObject`s, where the filename is the cryptographic hash (`VCOBjectHash`) of the object's contents
-- * `<storePath>/heads` is a set of current HEAD objects of the store, which can be seen as the roots of the VC tree. Each filename is the hash of an object,
--    and the file's contents are all the predecessors of this object, in chronological order, starting from the time it was created or cloned.
-- * `<storePath>/to_head` is a map from every `VCOBjectHash` to its current HEAD, where the file name is the source hash and the contents of the file are the HEAD hash
-- * `<storePath>/deps` is a map from every `VCOBjectHash` to its (transitive) dependencies, i.e. the file `<storePath>/deps/<hash>` describes the closure of `<hash>`
-- * Deleting `VCMeta VCObject` - Delete is implemented as soft delete. Object is moved to a directory called `removed`. Object's preds are also removed.
--   When an object is removed, its directory structure is preserved so you can undo it easily. i.e. `removed` directory has the same structure as `vc_store` directory.
module Inferno.VersionControl.Operations.Filesystem
  ( InfernoVCFilesystemEnv (..),
    InfernoVCFilesystemM,
    runInfernoVCFilesystemM,
    initVCStore,
  )
where

import Control.Concurrent.FairRWLock (RWLock)
import qualified Control.Concurrent.FairRWLock as RWL
import Control.Exception (throwIO)
import Control.Monad (foldM, forM, forM_)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, bracket_)
import Control.Monad.Error.Lens (catching, throwing)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT, asks, runReaderT)
import Crypto.Hash (digestFromByteString)
import Data.Aeson (FromJSON, ToJSON, Value, eitherDecode, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Contravariant (contramap)
import Data.Generics.Product (HasType, getTyped)
import Data.Generics.Sum (AsType (..))
import qualified Data.Set as Set
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Inferno.Types.Syntax (Dependencies (..))
import Inferno.VersionControl.Log (VCServerTrace (..), vcServerTraceToText)
import Inferno.VersionControl.Operations (InfernoVCOperations (..))
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

data InfernoVCFilesystemEnv = InfernoVCFilesystemEnv
  { storePath :: VCStorePath,
    tracer :: IOTracer VCServerTrace,
    lock :: RWLock
  }
  deriving (Generic)

newtype InfernoVCFilesystemM err m a = InfernoVCFilesystemM (ReaderT InfernoVCFilesystemEnv (ExceptT err m) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError err,
      MonadReader InfernoVCFilesystemEnv,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

runInfernoVCFilesystemM :: InfernoVCFilesystemM err m a -> InfernoVCFilesystemEnv -> ExceptT err m a
runInfernoVCFilesystemM (InfernoVCFilesystemM f) = runReaderT f

initVCStore :: FilePath -> IOTracer Text -> IO InfernoVCFilesystemEnv
initVCStore storePath txtTracer = do
  createDirectoryIfMissing True $ storePath </> "heads"
  createDirectoryIfMissing True $ storePath </> "to_head"
  createDirectoryIfMissing True $ storePath </> "deps"
  lock <- RWL.new
  let tracer = contramap vcServerTraceToText txtTracer
  pure InfernoVCFilesystemEnv {storePath = VCStorePath storePath, tracer, lock}

instance (MonadIO m, MonadMask m, AsType VCStoreError err) => InfernoVCOperations err (InfernoVCFilesystemM err m) where
  type Serializable (InfernoVCFilesystemM err m) = ToJSON
  type Deserializable (InfernoVCFilesystemM err m) = FromJSON

  storeVCObject obj@VCMeta {obj = ast, pred = p} = do
    VCStorePath storePath <- asks getTyped
    lock <- asks getTyped
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

    withWrite lock $ do
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
              forM_ (obj_h : preds) (\pred_h -> writeBS (storePath </> "to_head" </> show pred_h) obj_h_bs)

              pure obj_h
            else throwError $ TryingToAppendToNonHead pred_hash
        Nothing -> do
          obj_h <- writeHashedJSON storePath obj
          -- as there is no previous HEAD for this object, we simply create a new one
          let new_head_fp = storePath </> "heads" </> show obj_h
          appendBS new_head_fp mempty

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

  -- \| Delete a temporary object from the VC. This is used for autosaved scripts
  -- and to run tests against unsaved scripts
  deleteAutosavedVCObject obj_hash = do
    VCStorePath storePath <- asks getTyped
    lock <- asks getTyped
    withWrite lock $ do
      -- check if object meta exists with hash meta_hash, and get meta
      (VCMeta {name = obj_name} :: VCMeta Value Value VCObject) <- fetchVCObject obj_hash
      -- check that it is safe to delete
      if obj_name == pack "<AUTOSAVE>"
        then do
          -- delete object, object meta, head/to_head, and deps
          deleteFile $ storePath </> show obj_hash
          deleteFile $ storePath </> "heads" </> show obj_hash
          deleteFile $ storePath </> "to_head" </> show obj_hash
          deleteFile $ storePath </> "deps" </> show obj_hash
        else throwError $ TryingToDeleteNonAutosave obj_name
    where
      deleteFile fp = do
        trace $ DeleteFile fp
        liftIO $ removeFile fp

  -- \| Soft delete script and its history (both predecessors and successors).
  -- All scripts and their references are moved to "removed" directory
  deleteVCObjects obj_hash = do
    VCStorePath storePath <- asks getTyped
    lock <- asks getTyped
    liftIO $ do
      createDirectoryIfMissing True $ storePath </> "removed"
      createDirectoryIfMissing True $ storePath </> "removed" </> "heads"
      createDirectoryIfMissing True $ storePath </> "removed" </> "to_head"
      createDirectoryIfMissing True $ storePath </> "removed" </> "deps"

    withWrite lock $ do
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

  fetchVCObject = fetchVCObject' Nothing

  -- \| Fetch all objects that are public or that belong to the given set of groups.
  -- Note this is a potentially long operation so no locks are held while traversing the
  -- store and checking every object -- making this operation weakly consistent.
  -- This means the returned list does not necessarily reflect the state of the store at any
  -- point in time.
  fetchFunctionsForGroups grps = do
    heads <- getAllHeads
    foldM
      ( \objs hsh ->
          -- Since we don't hold a lock, some heads might have been deleted in the meantime
          -- so we catch and ignore CouldNotFindPath errors:
          catching (_Typed @VCStoreError) (checkGroupAndAdd objs hsh) (ignoreNotFounds objs)
      )
      []
      heads
    where
      checkGroupAndAdd objs hsh = do
        meta@VCMeta {obj, visibility, group} <- fetchVCObject hsh
        pure $ case obj of
          VCFunction _ _ ->
            if visibility == VCObjectPublic || group `Set.member` grps
              then fmap (const hsh) meta : objs
              else objs
          _ -> objs
      ignoreNotFounds objs (CouldNotFindPath _) = pure objs
      ignoreNotFounds _ e = throwError e

  fetchVCObjectHistory h = do
    VCStorePath storePath <- asks getTyped
    lock <- asks getTyped
    history <- withRead lock $ do
      head_h <- fetchCurrentHead h
      let head_fp = storePath </> "heads" </> show head_h
      preds <- readVCObjectHashTxt head_fp
      -- Order: newest to oldest
      pure $ head_h : reverse preds
    -- We recruse through history newest to oldest, and return the history in the same order:
    getHistory history
    where
      -- Recurse through history, newest to oldest, and stop when we find a clone
      getHistory (hsh : history) = do
        getObj hsh >>= \case
          Nothing -> getHistory history
          Just eObj -> do
            -- Assuming either the entire history of a script is deleted, or none of it,
            -- we only care about whether a script has been deleted when we look up the
            -- source of a clone
            let obj = either id id eObj
            case Inferno.VersionControl.Types.pred obj of
              CloneOf hsh' -> do
                -- if it is a clone, we would like to prepend source of the cloned script as part of the history.
                -- it is fine to only do this once since we only show the last source of the clone
                -- i.e. original -> cloneof orignal = cloned -> cloneof cloned = cloned'
                -- when viewing cloned' history, it will only show up to cloned.
                getObj hsh' >>= \case
                  Just (Right ori) ->
                    pure [obj, ori]
                  Just (Left ori) ->
                    -- The source of the clone script has been deleted, so we alter its 'pred' field as 'CloneOfRemoved' but
                    -- with the same hash. This way the upstream system (e.g. onping/frontend) can differentiate between
                    -- source that is still available and no longer available.
                    -- This does not change the way the script is persisted in the db, it is still stored as 'CloneOf'.
                    -- See 'CloneOfRemoved' for details.
                    pure [obj {Inferno.VersionControl.Types.pred = CloneOfRemoved hsh'}, ori]
                  Nothing ->
                    -- This script no longer exists even in 'removed' directory. The directory might get cleaned up by accident or something.
                    -- There are two choices we can make,
                    -- 1. Return a `VCMeta VCObjectHash` with dummy data
                    -- 2. Ignore this meta.
                    -- Approach no. 2 is taken here
                    getHistory history >>= \res -> pure $ obj : res
              _ -> getHistory history >>= \res -> pure $ obj : res
      getHistory [] = pure []

      getObj hsh = do
        VCStorePath storePath <- asks getTyped
        existsInRoot <- liftIO $ doesFileExist $ storePath </> show hsh
        if existsInRoot
          then do
            obj <- fmap (const hsh) <$> fetchVCObject hsh
            pure $ Just $ Right obj
          else do
            existsInRemoved <- liftIO $ doesFileExist $ storePath </> "removed" </> show hsh
            if existsInRemoved
              then do
                obj <- fmap (const hsh) <$> fetchRemovedVCObject hsh
                pure $ Just $ Left obj
              else pure Nothing

  -- NOTE: this is done without holding a lock, as dependencies are never modified.
  fetchVCObjectClosureHashes h = do
    VCStorePath storePath <- asks getTyped
    let fp = storePath </> "deps" </> show h
    readVCObjectHashTxt fp

  getAllHeads = do
    VCStorePath storePath <- getTyped <$> ask
    -- We don't need a lock here because this only lists the heads/ directory, it doesn't
    -- read any file contents (and I assume the `ls` is atomic)
    headsRaw <- liftIO $ getDirectoryContents $ storePath </> "heads"
    pure $
      foldr
        ( \hd xs ->
            case (either (const Nothing) Just $ Base64.decode $ Char8.pack hd) >>= digestFromByteString of
              Just hsh -> (VCObjectHash hsh) : xs
              Nothing -> xs
        )
        []
        (map takeFileName headsRaw)

fetchCurrentHead ::
  ( MonadError err m,
    AsType VCStoreError err,
    HasType (IOTracer VCServerTrace) env,
    HasType VCStorePath env,
    HasType RWLock env,
    MonadMask m,
    MonadIO m,
    MonadReader env m
  ) =>
  VCObjectHash ->
  m VCObjectHash
fetchCurrentHead h = do
  VCStorePath storePath <- asks getTyped
  lock <- asks getTyped
  let fp = storePath </> "to_head" </> show h
  withRead lock $ do
    exists <- liftIO $ doesFileExist fp
    if exists
      then
        readVCObjectHashTxt fp >>= \case
          [h'] -> pure h'
          _ -> throwError $ CouldNotFindHead h
      else throwError $ CouldNotFindHead h

newtype VCStorePath = VCStorePath FilePath deriving (Generic)

type VCStoreErrM err m = (AsType VCStoreError err, MonadError err m, MonadIO m)

type VCStoreLogM env m = (HasType (IOTracer VCServerTrace) env, MonadReader env m, MonadIO m)

type VCStoreEnvM env m = (HasType VCStorePath env, MonadReader env m, MonadIO m)

type VCStoreLockM env m = (HasType RWLock env, MonadReader env m, MonadIO m, MonadMask m)

withWrite :: (MonadIO m, MonadMask m) => RWLock -> m a -> m a
withWrite lock = bracket_ (liftIO $ RWL.acquireWrite lock) (liftIO $ RWL.releaseWrite lock >>= either throwIO return)

withRead :: (MonadIO m, MonadMask m) => RWLock -> m a -> m a
withRead lock = bracket_ (liftIO $ RWL.acquireRead lock) (liftIO $ RWL.releaseRead lock >>= either throwIO return)

trace :: VCStoreLogM env m => VCServerTrace -> m ()
trace t = do
  tracer <- getTyped <$> ask
  traceWith @IOTracer tracer t

throwError :: (VCStoreLogM env m, VCStoreErrM err m) => VCStoreError -> m a
throwError e = do
  trace $ ThrownVCStoreError e
  throwing _Typed e

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

-- | Fetch object from removed directory
fetchRemovedVCObject :: (VCStoreLogM env m, VCStoreErrM err m, VCStoreEnvM env m, VCStoreLockM env m, FromJSON a, FromJSON g) => VCObjectHash -> m (VCMeta a g VCObject)
fetchRemovedVCObject = fetchVCObject' (Just "removed")

fetchVCObject' :: (VCStoreLogM env m, VCStoreErrM err m, VCStoreEnvM env m, VCStoreLockM env m, FromJSON a, FromJSON g) => Maybe FilePath -> VCObjectHash -> m (VCMeta a g VCObject)
fetchVCObject' mprefix h = do
  VCStorePath storePath <- asks getTyped
  lock <- asks getTyped
  let fp = case mprefix of
        Nothing -> storePath </> show h
        Just prefix -> storePath </> prefix </> show h
  withRead lock $ do
    checkPathExists fp
    trace $ ReadJSON fp
    either (throwError . CouldNotDecodeObject h) pure =<< liftIO (eitherDecode <$> BL.readFile fp)
