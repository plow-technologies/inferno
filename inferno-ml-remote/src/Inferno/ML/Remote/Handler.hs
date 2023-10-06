{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Inferno.ML.Remote.Handler
  ( runInferenceHandler,
  )
where

import Control.Monad (when, (<=<))
import Control.Monad.Catch (throwM)
import Control.Monad.Extra (loopM, unlessM, whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.ListM (sortByM)
import Control.Monad.Reader.Class (asks)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Generics.Wrapped (wrappedTo)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Database.PostgreSQL.Simple (Only (Only, fromOnly), Query, withTransaction)
import Database.PostgreSQL.Simple.LargeObjects
  ( IOMode (ReadMode),
    Oid,
    loClose,
    loOpen,
    loRead,
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Inferno.Core (Interpreter (Interpreter, defaultEnv, evalExpr))
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
    Model,
    ModelCache,
    RemoteError
      ( CacheSizeExceeded,
        NoSuchModel,
        NoSuchParameter,
        OtherError
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
import System.FilePath (joinPath, takeDirectory)
import UnliftIO (MonadUnliftIO (withRunInIO))
import UnliftIO.Directory
  ( doesPathExist,
    getAccessTime,
    getFileSize,
    listDirectory,
    removeFile,
    withCurrentDirectory,
  )
import UnliftIO.Exception (bracket)
import UnliftIO.IO.File (writeBinaryFile)

runInferenceHandler ::
  Interpreter MlValue -> InferenceRequest -> RemoteM InferenceResponse
runInferenceHandler interpreter req = do
  script <- view #script <$> getParameter param uid
  ast <- liftEither500 $ mkFinalAst interpreter script
  cache <- asks $ view #cache
  wd <- req ^. #model & getAndCacheModel cache & fmap takeDirectory
  -- Change working directories to the model cache so that Hasktorch
  -- can find the models using relative paths (otherwise the AST would need
  -- to be updated to use an absolute path)
  withCurrentDirectory wd $ runEval interpreter ast
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

getParameter :: Id InferenceParam -> User -> RemoteM InferenceParam
getParameter iid u =
  firstOrThrow (NoSuchParameter iid)
    =<< queryStore q (u, iid)
  where
    q :: Query
    q = [sql| SELECT * FROM params WHERE id = ? AND "user" = ? LIMIT 1|]

getModelSize :: Oid -> RemoteM Integer
getModelSize oid =
  fmap fromOnly $
    firstOrThrow (OtherError "Could not get model size")
      =<< queryStore q (Only oid)
  where
    q :: Query
    q =
      [sql|
        SELECT sum(length(lo.data)), 0 FROM pg_largeobject lo
        WHERE lo.loid = ?
      |]

-- | Takes a model from the model store specified by name and adds it to the model
-- cache, evicting the older previously saved model(s) if the cache 'maxSize' will
-- be exceeded by adding the new model. If the model is already cached, it sets
-- the access time
getAndCacheModel :: ModelCache -> RequestedModel -> RemoteM FilePath
getAndCacheModel cache rm = do
  unlessM (doesPathExist path) $ do
    model <- queryModel
    (size, contents) <- model ^. #contents & getModelContents
    checkCacheSize size
    writeBinaryFile path contents
  pure path
  where
    getModelContents :: Oid -> RemoteM (Integer, ByteString)
    getModelContents m =
      asks (view #store) >>= \conn -> withRunInIO $ \r ->
        withTransaction conn . r $ do
          size <- getModelSize m
          bs <- liftIO . bracket (loOpen conn m ReadMode) (loClose conn) $
            \fd -> loRead conn fd $ fromIntegral size
          pure (size, bs)

    queryModel :: RemoteM Model
    queryModel =
      firstOrThrow (NoSuchModel (view #name rm))
        =<< queryStore q (rm ^. #name, rm ^. #version)
      where
        -- NOTE
        -- In the future, users will be able to add their own models. This is not
        -- going to be supported in the intial version, hence the `NULL` filter on
        -- `users`
        q :: Query
        q =
          [sql|
            SELECT * FROM models
            WHERE name = ?
            AND version = ?
            AND "user" IS NULL
          |]

    -- Cache the model under its specific version, i.e. `/<version>/<name>`
    path :: FilePath
    path =
      joinPath
        [ cache ^. #path,
          rm ^. #version . unpacked,
          rm ^. #name . to wrappedTo . unpacked
        ]

    maxSize :: Integer
    maxSize = cache ^. #maxSize & fromIntegral

    checkCacheSize :: Integer -> RemoteM ()
    checkCacheSize modelSize = do
      when (modelSize >= maxSize) $ throwM CacheSizeExceeded
      whenM cacheSizeExceeded evictOldModels
      where
        evictOldModels :: RemoteM ()
        evictOldModels = loopM doEvict =<< modelsByAccessTime (view #path cache)

        doEvict :: [FilePath] -> RemoteM (Either [FilePath] ())
        doEvict = \case
          [] -> pure $ Right ()
          m : ms ->
            cacheSizeExceeded >>= \case
              False -> pure $ Right ()
              True -> Left ms <$ removeFile m

        cacheSizeExceeded :: RemoteM Bool
        cacheSizeExceeded = (>= maxSize) <$> newCacheSize

        newCacheSize :: RemoteM Integer
        newCacheSize = (+ modelSize) <$> getFileSize (view #path cache)

modelsByAccessTime :: forall m. MonadIO m => FilePath -> m [FilePath]
modelsByAccessTime = sortByM compareAccessTime <=< listDirectory
  where
    compareAccessTime :: FilePath -> FilePath -> m Ordering
    compareAccessTime f1 f2 =
      getAccessTime f1 >>= \t1 ->
        compare t1 <$> getAccessTime f2
