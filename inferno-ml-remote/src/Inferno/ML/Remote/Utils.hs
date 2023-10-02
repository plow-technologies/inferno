{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Remote.Utils
  ( mkFinalAst,
    cacheAndUseModel,
    collectModelNames,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (when, (<=<))
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Extra (loopM, unlessM, whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.ListM (sortByM)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString as ByteString
import Data.Function ((&))
import Data.Generics.Labels ()
import qualified Data.HexString as HexString
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Only (Only), query)
import Inferno.Core (Interpreter (Interpreter, parseAndInferTypeReps))
import Inferno.ML.Remote.Types
  ( InfernoMlRemoteError (CacheSizeExceeded, NoSuchModel),
    ModelCache,
    ModelName (ModelName),
    ModelRow,
    ModelStore (Paths, Postgres),
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
import Lens.Micro.Platform ((^.))
import System.FilePath ((</>))
import UnliftIO.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesFileExist,
    getAccessTime,
    getFileSize,
    listDirectory,
    removeFile,
    setAccessTime,
  )
import UnliftIO.IO.File (writeBinaryFile)

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
    MonadThrow m
  ) =>
  ModelName ->
  ModelCache ->
  ModelStore ->
  m ()
cacheAndUseModel mn@(ModelName modelName) cache = \case
  Paths src -> do
    unlessM (doesFileExist srcPath) . throwM $ NoSuchModel mn
    cache ^. #path & createDirectoryIfMissing True
    ifNotCached $ do
      checkCacheSize =<< getFileSize srcPath
      copyFile srcPath cachedPath
    where
      srcPath :: FilePath
      srcPath = src </> Text.unpack modelName
  Postgres conn -> do
    liftIO q >>= \case
      [] -> throwM $ NoSuchModel mn
      model : _ -> ifNotCached $ do
        -- TODO
        -- Use a PG function to get the length, probably more efficient
        model ^. #model & ByteString.length & fromIntegral & checkCacheSize
        -- The default representation for Postgres' `bytea` type is hex encoding,
        -- so it will need to be converted back into bytes
        model ^. #model
          & HexString.hexString
          & HexString.toBytes
          & writeBinaryFile cachedPath
    where
      q :: IO [ModelRow]
      q =
        query conn "SELECT * FROM models WHERE name = ? AND user = NULL" $
          Only modelName
  where
    ifNotCached :: m () -> m ()
    ifNotCached f =
      doesFileExist cachedPath
        >>= \case
          -- This also sets the access time for the model to make sure that
          -- the least-recently-used models can be evicted if necessary
          True -> setAccessTime cachedPath =<< liftIO getCurrentTime
          -- Moving to the cache will implicitly set the access time
          False -> f

    checkCacheSize :: Integer -> m ()
    checkCacheSize modelSize = do
      when (modelSize >= maxSize) $ throwM CacheSizeExceeded
      whenM cacheSizeExceeded evictOldModels
      where
        evictOldModels :: m ()
        evictOldModels = loopM doEvict =<< modelsByAccessTime cacheDir

        doEvict :: [FilePath] -> m (Either [FilePath] ())
        doEvict = \case
          [] -> pure $ Right ()
          m : ms ->
            cacheSizeExceeded >>= \case
              False -> pure $ Right ()
              True -> Left ms <$ removeFile m

        cacheSizeExceeded :: m Bool
        cacheSizeExceeded = (>= maxSize) <$> newCacheSize

        newCacheSize :: m Integer
        newCacheSize = (+ modelSize) <$> getFileSize cacheDir

    maxSize :: Integer
    maxSize = cache ^. #maxSize & fromIntegral

    cacheDir :: FilePath
    cacheDir = cache ^. #path

    cachedPath :: FilePath
    cachedPath = cacheDir </> Text.unpack modelName

modelsByAccessTime :: forall m. MonadIO m => FilePath -> m [FilePath]
modelsByAccessTime = sortByM compareAccessTime <=< listDirectory
  where
    compareAccessTime :: FilePath -> FilePath -> m Ordering
    compareAccessTime f1 f2 =
      getAccessTime f1 >>= \t1 ->
        compare t1 <$> getAccessTime f2

-- Get the names of models used with @ML.loadModel@ so they can be cached
collectModelNames :: Expr a b -> [ModelName]
collectModelNames x = ModelName <$> collect mempty x
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
