{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Inferno.ML.Remote.Handler
  ( runInferenceHandler,
  )
where

import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.ListM (sortByM)
import Control.Monad.Reader.Class (asks)
import Data.Bifunctor (Bifunctor (first))
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Text as Text
import Inferno.Core (Interpreter (Interpreter, defaultEnv, evalExpr))
import Inferno.ML.Remote.Handler.Utils
  ( liftEither500,
    mkFinalAst,
  )
import Inferno.ML.Remote.Types
  ( InferenceRequest,
    InferenceResponse (InferenceResponse),
    ModelName (ModelName),
    RemoteM,
    SomeInfernoError (SomeInfernoError),
  )
import Inferno.ML.Types.Value (MlValue)
import Inferno.Types.Syntax (Expr, SourcePos)
import Inferno.Types.VersionControl (VCObjectHash)
import Inferno.Utils.Prettyprinter (renderPretty)
import Lens.Micro.Platform (view, (^.))
import UnliftIO.Directory
  ( getAccessTime,
    listDirectory,
    withCurrentDirectory,
  )

runInferenceHandler ::
  Interpreter MlValue -> InferenceRequest -> RemoteM InferenceResponse
runInferenceHandler interpreter req = do
  -- FIXME
  script <- req ^. #parameter & undefined
  ast <- liftEither500 $ mkFinalAst interpreter script
  cache <- asks $ view #cache
  -- FIXME
  cacheAndUseModel undefined
  -- Change working directories to the model cache so that Hasktorch
  -- can find the models using relative paths (otherwise the AST would need
  -- to be updated to use an absolute path)
  withCurrentDirectory (cache ^. #path) $
    runEval interpreter ast
  where
    runEval ::
      Interpreter MlValue ->
      Expr (Maybe VCObjectHash) SourcePos ->
      RemoteM InferenceResponse
    runEval Interpreter {evalExpr, defaultEnv} ast =
      fmap (coerce . Text.strip . renderPretty) . liftEither500 . first SomeInfernoError
        =<< liftIO (evalExpr defaultEnv Map.empty ast)

-- | Takes a model from the model store specified by name and adds it to the model
-- cache, evicting the older previously saved model(s) if the cache 'maxSize' will
-- be exceeded by adding the new model. If the model is already cached, it sets
-- the access time
cacheAndUseModel :: ModelName -> RemoteM ()
cacheAndUseModel (ModelName _) = undefined

modelsByAccessTime :: forall m. MonadIO m => FilePath -> m [FilePath]
modelsByAccessTime = sortByM compareAccessTime <=< listDirectory
  where
    compareAccessTime :: FilePath -> FilePath -> m Ordering
    compareAccessTime f1 f2 =
      getAccessTime f1 >>= \t1 ->
        compare t1 <$> getAccessTime f2
