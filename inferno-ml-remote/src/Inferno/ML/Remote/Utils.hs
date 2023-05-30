module Inferno.ML.Remote.Utils
  ( mkFinalAst,
    typecheck,
    cacheAndUseModel,
    collectModelNames,
  )
where

import Control.Monad ((<=<))
import Control.Monad.Catch (MonadMask, MonadThrow (throwM), bracket)
import Control.Monad.Extra (loopM, unlessM, whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Coerce (coerce)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Generics.Product (HasType (typed))
import Data.List (sortOn)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Tuple.Extra (dupe, fst3, snd3, (&&&))
import Data.Word (Word64)
import Inferno.Infer (TypeError, inferExpr, inferTypeReps)
import Inferno.Infer.Pinned (pinExpr)
import Inferno.ML.Module.Prelude
  ( baseOpsTable,
    builtinModules,
    builtinModulesOpsTable,
    builtinModulesPinMap,
  )
import Inferno.ML.Remote.Types
  ( InfernoMlRemoteError
      ( CacheSizeExceeded,
        ExternalProcessFailed,
        NoSuchModel
      ),
    ModelCache,
    ModelCacheOption (CompressedPaths, Paths),
    Script (Script),
    SomeInfernoError (SomeInfernoError),
  )
import Inferno.Parse (parseExpr)
import Inferno.Types.Syntax
  ( Expr (App, Bracketed, Let, Lit, OpenModule, TypeRep, Var),
    ExtIdent (ExtIdent),
    ImplExpl (Expl),
    Lit (LText),
    SourcePos,
    collectArrs,
  )
import Inferno.Types.Type (ImplType, InfernoType, TCScheme, TypeClass)
import Inferno.Types.VersionControl (Pinned, VCObjectHash, pinnedToMaybe)
import Lens.Micro.Platform (each, view, (^.), (^..))
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesFileExist,
    getAccessTime,
    getFileSize,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
    setAccessTime,
  )
import System.Exit (ExitCode (ExitFailure))
import System.FilePath (takeFileName, (<.>), (</>))
import System.IO.Temp (createTempDirectory)
import System.Process.Typed (proc, runProcess)

mkFinalAst ::
  ( Expr (Pinned VCObjectHash) SourcePos,
    TCScheme
  ) ->
  Either SomeInfernoError (Expr (Maybe VCObjectHash) ())
mkFinalAst (ast, tcscheme) = mkFinal <$> first SomeInfernoError (runtimeReps tys)
  where
    mkFinal :: [InfernoType] -> Expr (Maybe VCObjectHash) ()
    mkFinal =
      foldl' App (bimap pinnedToMaybe (const ()) ast)
        . fmap (TypeRep ())

    runtimeReps ::
      ([InfernoType], InfernoType) ->
      Either [TypeError SourcePos] [InfernoType]
    runtimeReps = uncurry $ inferTypeReps allClasses tcscheme

    tys :: ([InfernoType], InfernoType)
    tys =
      tcscheme ^. typed @ImplType . typed @InfernoType
        & collectArrs
        & (init &&& last)
    allClasses :: Set TypeClass
    allClasses = builtinModules ^.. each . #moduleTypeClasses & Set.unions

typecheck ::
  Script ->
  Either
    SomeInfernoError
    ( Expr (Pinned VCObjectHash) SourcePos,
      TCScheme
    )
typecheck =
  first SomeInfernoError . fmap (fst3 &&& snd3) . inferExpr builtinModules
    <=< first SomeInfernoError . pinExpr builtinModulesPinMap
    <=< first SomeInfernoError
      . fmap fst
      . parseExpr baseOpsTable builtinModulesOpsTable
      . coerce

-- | Takes a model from the model store specified by name and adds it to the model
-- cache, evicting the older previously saved model(s) if the cache 'maxSize' will
-- be exceeded by adding the new model. If the model is already cached, it sets
-- the access time
cacheAndUseModel ::
  forall m.
  ( MonadIO m,
    MonadThrow m,
    MonadMask m
  ) =>
  Text ->
  ModelCacheOption ->
  m ()
cacheAndUseModel model = \case
  Paths src cache -> do
    unlessM (liftIO (doesFileExist srcPath)) . throwM $ NoSuchModel model
    cache ^. #path & liftIO . createDirectoryIfMissing True
    liftIO (doesFileExist cachedPath) >>= \case
      -- This also sets the access time for the model to make sure that
      -- the least-recently-used models can be evicted if necessary
      True -> liftIO (setAccessTime cachedPath =<< getCurrentTime)
      -- Moving to the cache will implicitly set the access time
      False -> moveToCache cache srcPath
    where
      cachedPath :: FilePath
      cachedPath = cache ^. #path & (</> Text.unpack model)

      srcPath :: FilePath
      srcPath = src </> Text.unpack model
  -- TODO
  -- There's certainly a more efficient way to do this that doesn't require
  -- an intermediate temp directory, but this makes it easier to share the
  -- rest of the code with the uncompressed path option
  CompressedPaths src cache -> do
    unlessM (liftIO (doesFileExist srcCompressed)) . throwM $ NoSuchModel model
    cache ^. #path & liftIO . createDirectoryIfMissing True
    liftIO (doesFileExist cachedPath) >>= \case
      True -> liftIO (setAccessTime cachedPath =<< getCurrentTime)
      -- NOTE
      -- The explicit `bracket` is required here. `withSystemTempDirectory` can't
      -- be used since it requires and inner IO action. If `Handler` had an
      -- `UnliftIO` instance, that could be used instead, but it doesn't
      False ->
        bracket
          (liftIO (createTempDirectory "/tmp" "inferno-ml-remote"))
          (liftIO . removeDirectoryRecursive)
          $ moveToCache cache <=< decompress
    where
      decompress :: FilePath -> m FilePath
      decompress tmp =
        runProcess (proc "unzstd" [srcCompressed, "-q", "--output-dir-flat", tmp])
          >>= \case
            ExitFailure c -> throwM $ ExternalProcessFailed "unzstd" c
            _ -> pure $ tmp </> Text.unpack model

      cachedPath :: FilePath
      cachedPath = cache ^. #path & (</> Text.unpack model)

      srcCompressed :: FilePath
      srcCompressed = src </> Text.unpack model <.> "zst"
  where
    moveToCache :: ModelCache -> FilePath -> m ()
    moveToCache cache path = do
      whenM ((>= view #maxSize cache) <$> newModelSize) $
        throwM CacheSizeExceeded
      whenM cacheSizeExceeded evictOldModels
      liftIO $ copyFile path modelPath
      where
        evictOldModels :: m ()
        evictOldModels = loopM doEvict =<< modelsByATime cacheDir

        doEvict :: [FilePath] -> m (Either [FilePath] ())
        doEvict = \case
          [] -> pure $ Right ()
          m : ms ->
            cacheSizeExceeded >>= \case
              False -> pure $ Right ()
              True -> Left ms <$ liftIO (removeFile m)

        newModelSize :: m Word64
        newModelSize = fromIntegral <$> liftIO (getFileSize path)

        cacheSizeExceeded :: m Bool
        cacheSizeExceeded =
          fmap (>= view #maxSize cache) . newCacheSize
            =<< newModelSize

        newCacheSize :: Word64 -> m Word64
        newCacheSize newSize =
          (+ newSize) . fromIntegral <$> liftIO (getFileSize cacheDir)

        modelPath :: FilePath
        modelPath = cacheDir </> takeFileName path

        cacheDir :: FilePath
        cacheDir = cache ^. #path

modelsByATime :: MonadIO m => FilePath -> m [FilePath]
modelsByATime dir =
  liftIO $
    fmap (fmap snd . sortOn fst)
      . traverse getATime
      . fmap (dupe . (dir </>))
      =<< listDirectory dir
  where
    getATime :: (FilePath, FilePath) -> IO (UTCTime, FilePath)
    getATime (_, p) = (,p) <$> getAccessTime p

-- Get the names of models used with @ML.loadModel@ so they can be cached
collectModelNames :: Expr a () -> [Text]
collectModelNames = collect mempty
  where
    collect :: [Text] -> Expr a () -> [Text]
    collect models = \case
      OpenModule _ _ _ _ _ expr -> collect models expr
      Let _ _ _ _ lhs _ rhs ->
        collect mempty lhs <> collect models rhs
      App
        (Var _ _ _ (Expl (ExtIdent (Right "loadModel"))))
        (Lit _ (LText model)) ->
          model : models
      App lhs rhs -> collect mempty lhs <> collect models rhs
      Bracketed _ expr _ -> collect models expr
      _ -> models
