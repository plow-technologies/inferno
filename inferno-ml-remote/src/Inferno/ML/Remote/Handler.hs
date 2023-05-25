{-# LANGUAGE ViewPatterns #-}

module Inferno.ML.Remote.Handler
  ( runInferenceHandler,
  )
where

import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.Extra (loopM, whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (asks)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
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
import Data.Time (UTCTime)
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
  )
import Inferno.ML.Types.Value (MlValue)
import Inferno.Parse (parseExpr)
import Inferno.Types.Syntax (Expr (App, TypeRep), SourcePos, collectArrs)
import Inferno.Types.Type (ImplType, InfernoType, TCScheme, TypeClass)
import Inferno.Types.Value (ImplEnvM)
import Inferno.Types.VersionControl (Pinned, VCObjectHash, pinnedToMaybe)
import Inferno.Utils.Prettyprinter (renderPretty)
import Lens.Micro.Platform (each, view, (^.), (^..))
import Servant (ServerError (errBody), err400, err500)
import System.Directory
  ( copyFile,
    getAccessTime,
    getFileSize,
    listDirectory,
    removeFile,
  )
import System.FilePath (takeFileName, (</>))

runInferenceHandler :: Script -> InfernoMlRemoteM EvalResult
runInferenceHandler (Script src) =
  fmap (coerce . renderPretty) . liftEither500
    =<< liftIO . runEvalIO mkEnv mempty
    =<< mkFinalAst
    =<< typecheck src
  where
    mkFinalAst ::
      ( Expr (Pinned VCObjectHash) SourcePos,
        TCScheme
      ) ->
      InfernoMlRemoteM (Expr (Maybe VCObjectHash) ())
    mkFinalAst (ast, tcscheme) = mkFinal <$> liftEither500 (runtimeReps tys)
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

    typecheck ::
      Text ->
      InfernoMlRemoteM
        ( Expr (Pinned VCObjectHash) SourcePos,
          TCScheme
        )
    typecheck =
      liftEither500 . fmap (fst3 &&& snd3) . inferExpr builtinModules
        <=< liftEither500 . pinExpr builtinModulesPinMap
        <=< liftEither500
          . fmap fst
          . parseExpr baseOpsTable builtinModulesOpsTable

    allClasses :: Set TypeClass
    allClasses = builtinModules ^.. each . #moduleTypeClasses & Set.unions

    mkEnv ::
      ImplEnvM
        (ExceptT EvalError IO)
        MlValue
        ( TermEnv VCObjectHash MlValue (ImplEnvM (ExceptT EvalError IO) MlValue)
        )
    mkEnv = (mempty,) . snd <$> builtinModulesTerms

liftEither500 :: forall e a. Show e => Either e a -> InfernoMlRemoteM a
liftEither500 = either (throwError . mk500) pure
  where
    mk500 :: Show e => e -> ServerError
    mk500 (ByteString.Lazy.Char8.pack . show -> e) =
      err500
        { errBody = "Script evalution failed with: " <> e
        }

-- | Takes a model from the model store specified by name and adds it to the model
-- cache, evicting the older previously saved model(s) if the cache 'maxSize' will
-- be exceeded by adding the new model
_cacheModel :: Text -> InfernoMlRemoteM FilePath
_cacheModel model =
  asks (view #modelCache) >>= \case
    Nothing ->
      throwError $
        err500
          { errBody = "No model cache has been configured for this server"
          }
    Just (Paths src cache) -> moveToCache cache $ src </> Text.unpack model
    Just _ -> undefined
  where
    moveToCache :: ModelCache -> FilePath -> InfernoMlRemoteM FilePath
    moveToCache cache path =
      modelPath <$ do
        whenM ((>= view #maxSize cache) <$> newModelSize) $
          throwError
            err400
              { errBody = "Model exceeds maximum size"
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

getATime :: (FilePath, FilePath) -> IO (UTCTime, FilePath)
getATime (_, p) = (,p) <$> getAccessTime p
