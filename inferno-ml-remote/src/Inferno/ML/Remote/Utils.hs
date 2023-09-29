{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Remote.Utils
  ( mkFinalAst,
    cacheAndUseModel,
    collectModelNames,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadMask, MonadThrow (throwM))
import Control.Monad.Extra (loopM, unlessM, whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (Bifunctor (first))
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Tuple.Extra (dupe)
import Data.Word (Word64)
import Inferno.Core (Interpreter (Interpreter, parseAndInferTypeReps))
import Inferno.ML.Remote.Types
  ( InfernoMlRemoteError
      ( CacheSizeExceeded,
        NoSuchModel
      ),
    ModelCache,
    ModelStore (Paths),
    Script (Script),
    SomeInfernoError (SomeInfernoError),
  )
import Inferno.ML.Types.Value (MlValue)
import Inferno.Types.Syntax
  ( Expr (App, Bracketed, Let, Lit, OpenModule, Var),
    ExtIdent (ExtIdent),
    ImplExpl (Expl),
    Lit (LText),
    Scoped (LocalScope),
    SourcePos,
  )
import Inferno.Types.VersionControl (VCObjectHash)
import Lens.Micro.Platform (view, (^.))
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesFileExist,
    getAccessTime,
    getFileSize,
    listDirectory,
    removeFile,
    setAccessTime,
  )
import System.FilePath (takeFileName, (</>))

mkFinalAst ::
  Interpreter MlValue ->
  Script ->
  Either
    SomeInfernoError
    (Expr (Maybe VCObjectHash) SourcePos)
mkFinalAst Interpreter {parseAndInferTypeReps} (Script src) =
  first SomeInfernoError . parseAndInferTypeReps $ src

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
  ModelCache ->
  ModelStore ->
  m ()
cacheAndUseModel model cache = \case
  Paths src -> do
    unlessM (liftIO (doesFileExist srcPath)) . throwM $ NoSuchModel model
    cache ^. #path & liftIO . createDirectoryIfMissing True
    liftIO (doesFileExist cachedPath) >>= \case
      -- This also sets the access time for the model to make sure that
      -- the least-recently-used models can be evicted if necessary
      True -> liftIO (setAccessTime cachedPath =<< getCurrentTime)
      -- Moving to the cache will implicitly set the access time
      False -> moveToCache srcPath
    where
      cachedPath :: FilePath
      cachedPath = cache ^. #path & (</> Text.unpack model)

      srcPath :: FilePath
      srcPath = src </> Text.unpack model
  where
    moveToCache :: FilePath -> m ()
    moveToCache path = do
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
collectModelNames :: Expr a b -> [Text]
collectModelNames x = collect mempty x
  where
    collect :: [Text] -> Expr a b -> [Text]
    collect models = \case
      App (Var _ _ _ (Expl (ExtIdent (Right "loadModel")))) lhs -> case lhs of
        Lit _ (LText model) -> model : models
        -- In this case, the original literal text needs to be recovered from
        -- the original AST
        Var _ _ LocalScope (Expl (ExtIdent (Right var))) ->
          maybe models (: models) $ findVar var x
        _ -> models
      App lhs rhs -> collect mempty lhs <> collect models rhs
      Let _ _ _ _ lhs _ rhs ->
        collect mempty lhs <> collect models rhs
      OpenModule _ _ _ _ _ expr -> collect models expr
      Bracketed _ expr _ -> collect models expr
      _ -> models

    -- Get the literal text (i.e. the name of the model) from the let-binding
    -- used subsequently with an application of `loadLodel`
    findVar :: Text -> Expr a b -> Maybe Text
    findVar var = \case
      Let _ _ (Expl (ExtIdent (Right ext))) _ (Lit _ (LText model)) _ _
        | ext == var -> pure model
      Let _ _ _ _ lhs _ rhs -> findVar var lhs <|> findVar var rhs
      App lhs rhs -> findVar var lhs <|> findVar var rhs
      OpenModule _ _ _ _ _ expr -> findVar var expr
      Bracketed _ expr _ -> findVar var expr
      _ -> Nothing
