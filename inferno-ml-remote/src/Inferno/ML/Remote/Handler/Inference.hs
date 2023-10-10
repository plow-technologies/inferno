{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Remote.Handler.Inference
  ( runInferenceHandler,
    getAndCacheModel,
    linkVersionedModel,
  )
where

import Control.Monad (when, (<=<))
import Control.Monad.Catch (throwM)
import Control.Monad.Extra (loopM, unlessM, whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.ListM (sortByM)
import Control.Monad.Reader.Class (asks)
import Data.Bifunctor (Bifunctor (first))
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Generics.Wrapped (wrappedTo)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Database.PostgreSQL.Simple
  ( Query,
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Inferno.Core (Interpreter (Interpreter, defaultEnv, evalExpr))
import Inferno.ML.Remote.Handler.Inference.Model (getModel, getModelContents)
import Inferno.ML.Remote.Handler.Utils
  ( firstOrThrow,
    liftEither500,
    mkFinalAst,
    queryStore,
  )
import Inferno.ML.Remote.Types
  ( Id,
    InferenceParam,
    InferenceRequest,
    InferenceResponse (InferenceResponse),
    ModelCache,
    RemoteError
      ( CacheSizeExceeded,
        NoSuchParameter
      ),
    RemoteM,
    RequestedModel,
    SomeInfernoError (SomeInfernoError),
    User,
  )
import Inferno.ML.Types.Value (MlValue)
import Inferno.Types.Syntax (Expr, SourcePos)
import Inferno.Types.VersionControl (VCObjectHash)
import Inferno.Utils.Prettyprinter (renderPretty)
import Lens.Micro.Platform (to, unpacked, view, (^.))
import System.FilePath (dropExtensions, (<.>))
import UnliftIO.Directory
  ( createFileLink,
    doesPathExist,
    getAccessTime,
    getFileSize,
    listDirectory,
    removeFile,
    removePathForcibly,
    withCurrentDirectory,
  )
import UnliftIO.IO.File (writeBinaryFileDurableAtomic)

runInferenceHandler :: InferenceRequest -> RemoteM InferenceResponse
runInferenceHandler req = do
  script <- view #script <$> getParameter param uid
  interpreter <- asks $ view #interpreter
  ast <- liftEither500 $ mkFinalAst interpreter script
  cache <- asks $ view #cache
  -- Change working directories to the model cache so that Hasktorch
  -- can find the models using relative paths (otherwise the AST would need
  -- to be updated to use an absolute versioned)
  withCurrentDirectory (cache ^. #path) $ do
    linkVersionedModel =<< getAndCacheModel cache (view #model req)
    runEval interpreter ast
  where
    runEval ::
      Interpreter MlValue ->
      Expr (Maybe VCObjectHash) SourcePos ->
      RemoteM InferenceResponse
    runEval Interpreter {evalExpr, defaultEnv} ast =
      fmap (coerce . Text.strip . renderPretty) . liftEither500 . first SomeInfernoError
        =<< liftIO (evalExpr defaultEnv Map.empty ast)

    param :: Id InferenceParam
    param = req ^. #parameter

    uid :: User
    uid = req ^. #user

-- Link the versioned versioned to the model, e.g. `<name>.<version>` to just
-- `<name>.ts.pt`, so it can be loaded by Hasktorch
linkVersionedModel :: FilePath -> RemoteM ()
linkVersionedModel withVersion = do
  whenM (doesPathExist withExt) $ removePathForcibly withExt
  createFileLink withVersion withExt
  where
    withExt :: FilePath
    withExt = dropExtensions withVersion <.> "ts" <.> "pt"

getParameter :: Id InferenceParam -> User -> RemoteM InferenceParam
getParameter iid u =
  firstOrThrow (NoSuchParameter iid)
    =<< queryStore q (iid, u)
  where
    q :: Query
    q = [sql| SELECT * FROM params WHERE id = ? AND "user" = ? LIMIT 1 |]

-- | Takes a model from the model store specified by name and version and adds
-- it to the model cache, evicting the older previously saved model(s) if the
-- cache 'maxSize' will be exceeded by adding the new model
getAndCacheModel :: ModelCache -> RequestedModel -> RemoteM FilePath
getAndCacheModel cache rm = do
  unlessM (doesPathExist versioned) $ do
    model <- getModel rm
    (size, contents) <- model ^. #contents & getModelContents
    checkCacheSize size
    writeBinaryFileDurableAtomic versioned contents
  pure versioned
  where
    -- Cache the model with its specific version, i.e. `<name>.ts.pt.<version>`, which
    -- will later be symlinked to `<name>.ts.pt`
    versioned :: FilePath
    versioned =
      rm ^. #name . to wrappedTo . unpacked
        & (<.> rm ^. #version . unpacked)

    maxSize :: Integer
    maxSize = cache ^. #maxSize & fromIntegral

    checkCacheSize :: Integer -> RemoteM ()
    checkCacheSize modelSize = do
      when (modelSize >= maxSize) $ throwM CacheSizeExceeded
      whenM cacheSizeExceeded evictOldModels
      where
        evictOldModels :: RemoteM ()
        evictOldModels = loopM doEvict =<< modelsByAccessTime "."

        doEvict :: [FilePath] -> RemoteM (Either [FilePath] ())
        doEvict = \case
          [] -> pure $ Right ()
          m : ms ->
            cacheSizeExceeded >>= \case
              False -> pure $ Right ()
              True -> Left ms <$ removeFile m

        cacheSizeExceeded :: RemoteM Bool
        cacheSizeExceeded = (>= maxSize) <$> newCacheSize
          where
            newCacheSize :: RemoteM Integer
            newCacheSize =
              fmap ((+ modelSize) . sum) . traverse getFileSize
                =<< listDirectory "."

modelsByAccessTime :: forall m. MonadIO m => FilePath -> m [FilePath]
modelsByAccessTime = sortByM compareAccessTime <=< listDirectory
  where
    compareAccessTime :: FilePath -> FilePath -> m Ordering
    compareAccessTime f1 f2 =
      getAccessTime f1 >>= \t1 ->
        compare t1 <$> getAccessTime f2
