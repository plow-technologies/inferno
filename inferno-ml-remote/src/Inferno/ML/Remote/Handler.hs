{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Inferno.ML.Remote.Handler
  ( runInferenceHandler,
  )
where

import Control.Exception (Exception (displayException))
import Control.Monad (unless)
import Control.Monad.Catch (bracket_)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (asks)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Generics.Product (HasType (typed))
import qualified Data.Map as Map
import qualified Data.Text as Text
import Inferno.Core (Interpreter (Interpreter, evalInEnv))
import Inferno.ML.Remote.Types
  ( EvalResult (EvalResult),
    InfernoMlRemoteM,
    ModelCache,
    Script,
    SomeInfernoError (SomeInfernoError),
  )
import Inferno.ML.Remote.Utils
  ( cacheAndUseModel,
    collectModelNames,
    mkFinalAst,
  )
import Inferno.ML.Types.Value (MlValue)
import Inferno.Types.Syntax (Expr, SourcePos)
import Inferno.Types.VersionControl (VCObjectHash)
import Inferno.Utils.Prettyprinter (renderPretty)
import Lens.Micro.Platform (view, (^.))
import Servant (ServerError (errBody), err500)
import System.Directory
  ( getCurrentDirectory,
    setCurrentDirectory,
  )

runInferenceHandler :: Script -> InfernoMlRemoteM EvalResult
runInferenceHandler src = do
  (interpreter, ast) <- liftEither500 $ mkFinalAst src
  cwd <- liftIO getCurrentDirectory
  asks (view #modelCache) >>= \case
    Nothing -> do
      unless (null (collectModelNames ast)) $
        throwError $
          err500
            { errBody = "No model cache has been configured for this server"
            }
      runEval interpreter ast
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
        $ runEval interpreter ast
  where
    runEval ::
      Interpreter MlValue ->
      Expr (Maybe VCObjectHash) SourcePos ->
      InfernoMlRemoteM EvalResult
    runEval Interpreter {evalInEnv} ast =
      fmap (coerce . Text.strip . renderPretty) . liftEither500 . first SomeInfernoError
        =<< liftIO (evalInEnv Map.empty Map.empty ast)

liftEither500 :: forall e a. Exception e => Either e a -> InfernoMlRemoteM a
liftEither500 = either (throwError . mk500) pure
  where
    mk500 :: Show e => e -> ServerError
    mk500 (ByteString.Lazy.Char8.pack . displayException -> e) =
      err500
        { errBody = "Script evalution failed with: " <> e
        }