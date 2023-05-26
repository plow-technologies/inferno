{-# LANGUAGE ViewPatterns #-}

module Inferno.ML.Remote.Handler
  ( runInferenceHandler,
    mkFinalAst,
    typecheck,
    collectModelNames,
  )
where

import Control.Exception (Exception (displayException))
import Control.Monad (unless, (<=<))
import Control.Monad.Catch (bracket_)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.Extra (loopM, whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (asks)
import Data.Bifunctor (Bifunctor (bimap, first))
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Coerce (coerce)
import Data.Foldable (foldl', traverse_)
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
import Inferno.Eval (TermEnv, runEvalIO)
import Inferno.Eval.Error (EvalError)
import Inferno.Infer (TypeError, inferExpr, inferTypeReps)
import Inferno.Infer.Pinned (pinExpr)
import Inferno.ML.Module.Prelude
  ( baseOpsTable,
    builtinModules,
    builtinModulesOpsTable,
    builtinModulesPinMap,
    builtinModulesTerms,
  )
import Inferno.ML.Remote.Types
  ( EvalResult (EvalResult),
    InfernoMlRemoteM,
    ModelCache,
    ModelCacheOption (Paths),
    Script (Script),
    SomeInfernoError (SomeInfernoError),
  )
import Inferno.ML.Types.Value (MlValue)
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
import Inferno.Types.Value (ImplEnvM)
import Inferno.Types.VersionControl (Pinned, VCObjectHash, pinnedToMaybe)
import Inferno.Utils.Prettyprinter (renderPretty)
import Lens.Micro.Platform (each, view, (^.), (^..))
import Servant (ServerError (errBody), err400, err500)
import System.Directory
  ( copyFile,
    doesFileExist,
    getAccessTime,
    getCurrentDirectory,
    getFileSize,
    listDirectory,
    removeFile,
    setAccessTime,
    setCurrentDirectory,
  )
import System.FilePath (takeFileName, (</>))

runInferenceHandler :: Script -> InfernoMlRemoteM EvalResult
runInferenceHandler src = do
  ast <- liftEither500 $ mkFinalAst =<< typecheck src
  cwd <- liftIO getCurrentDirectory
  asks (view #modelCache) >>= \case
    Nothing -> do
      unless (null (collectModelNames ast)) $
        throwError $
          err500
            { errBody = "No model cache has been configured for this server"
            }
      runEval ast
    Just cache -> do
      traverse_ (`cacheAndUseModel` cache) $ collectModelNames ast
      -- Change working directories to the model cache so that Hasktorch
      -- can find the models using relative paths (otherwise the AST would need
      -- to be updated to use an absolute path)
      --
      -- NOTE
      -- We can't use `withCurrentDirectory` here because it expects an IO action
      -- to run in between. And there's no `UnliftIO` instance for `Handler`
      -- (because it uses `ExceptT`), so it's easier just to `bracket` it
      -- directly
      bracket_
        (cache ^. typed @ModelCache . #path & liftIO . setCurrentDirectory)
        (cwd & liftIO . setCurrentDirectory)
        $ runEval ast
  where
    runEval :: Expr (Maybe VCObjectHash) () -> InfernoMlRemoteM EvalResult
    runEval ast =
      fmap (coerce . Text.strip . renderPretty) . liftEither500 . first SomeInfernoError
        =<< liftIO (runEvalIO mkEnv mempty ast)

    mkEnv ::
      ImplEnvM
        (ExceptT EvalError IO)
        MlValue
        ( TermEnv VCObjectHash MlValue (ImplEnvM (ExceptT EvalError IO) MlValue)
        )
    mkEnv = (mempty,) . snd <$> builtinModulesTerms

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

liftEither500 :: forall e a. Exception e => Either e a -> InfernoMlRemoteM a
liftEither500 = either (throwError . mk500) pure
  where
    mk500 :: Show e => e -> ServerError
    mk500 (ByteString.Lazy.Char8.pack . displayException -> e) =
      err500
        { errBody = "Script evalution failed with: " <> e
        }

-- | Takes a model from the model store specified by name and adds it to the model
-- cache, evicting the older previously saved model(s) if the cache 'maxSize' will
-- be exceeded by adding the new model. If the model is already cached, it sets
-- the access time
cacheAndUseModel :: Text -> ModelCacheOption -> InfernoMlRemoteM ()
cacheAndUseModel model = \case
  Paths src cache ->
    liftIO (doesFileExist cachedPath) >>= \case
      -- This also sets the access time for the model to make sure that
      -- the least-recently-used models can be evicted if necessary
      True -> liftIO (setAccessTime cachedPath =<< getCurrentTime)
      -- Moving to the cache will implicitly set the access time
      False -> moveToCache cache srcPath
    where
      srcPath :: FilePath
      srcPath = src </> Text.unpack model

      cachedPath :: FilePath
      cachedPath = cache ^. #path & (</> Text.unpack model)
  _ -> undefined
  where
    moveToCache :: ModelCache -> FilePath -> InfernoMlRemoteM ()
    moveToCache cache path = do
      whenM ((>= view #maxSize cache) <$> newModelSize) $
        throwError
          err400
            { errBody = "Model exceeds maximum cache size"
            }
      whenM cacheSizeExceeded evictOldModels
      liftIO $ copyFile path modelPath
      where
        evictOldModels :: InfernoMlRemoteM ()
        evictOldModels = loopM doEvict =<< modelsByATime dir

        doEvict ::
          [FilePath] ->
          InfernoMlRemoteM (Either [FilePath] ())
        doEvict = \case
          [] -> pure $ Right ()
          m : ms ->
            cacheSizeExceeded >>= \case
              False -> pure $ Right ()
              True -> Left ms <$ liftIO (removeFile m)

        newModelSize :: InfernoMlRemoteM Word64
        newModelSize = fromIntegral <$> liftIO (getFileSize path)

        cacheSizeExceeded :: InfernoMlRemoteM Bool
        cacheSizeExceeded =
          fmap (>= view #maxSize cache) . newCacheSize
            =<< newModelSize

        newCacheSize :: Word64 -> InfernoMlRemoteM Word64
        newCacheSize newSize =
          (+ newSize) . fromIntegral <$> liftIO (getFileSize dir)

        modelPath :: FilePath
        modelPath = dir </> takeFileName path

        dir :: FilePath
        dir = cache ^. #path

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
