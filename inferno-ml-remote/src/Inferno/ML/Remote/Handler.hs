{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Inferno.ML.Remote.Handler
  ( runInferenceHandler,
  )
where

import Control.Exception (Exception (displayException))
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (asks)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Text as Text
import Inferno.Core (Interpreter (Interpreter, defaultEnv, evalExpr))
import Inferno.ML.Remote.Types
  ( InferenceRequest,
    InferenceResponse (InferenceResponse),
    RemoteM,
    SomeInfernoError (SomeInfernoError),
  )
import Inferno.ML.Remote.Utils
  ( cacheAndUseModel,
    mkFinalAst,
  )
import Inferno.ML.Types.Value (MlValue)
import Inferno.Types.Syntax (Expr, SourcePos)
import Inferno.Types.VersionControl (VCObjectHash)
import Inferno.Utils.Prettyprinter (renderPretty)
import Lens.Micro.Platform (view, (^.))
import Servant (ServerError (errBody), err500)
import UnliftIO.Directory (withCurrentDirectory)

runInferenceHandler ::
  Interpreter MlValue -> InferenceRequest -> RemoteM InferenceResponse
runInferenceHandler interpreter req = do
  -- FIXME
  script <- req ^. #parameter & undefined
  ast <- liftEither500 $ mkFinalAst interpreter script
  cache <- asks $ view #modelCache
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

liftEither500 :: forall e a. Exception e => Either e a -> RemoteM a
liftEither500 = either (throwM . mk500) pure
  where
    mk500 :: Show e => e -> ServerError
    mk500 (ByteString.Lazy.Char8.pack . displayException -> e) =
      err500
        { errBody = "Script evalution failed with: " <> e
        }
