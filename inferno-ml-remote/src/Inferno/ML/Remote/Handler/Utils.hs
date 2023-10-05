{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Inferno.ML.Remote.Handler.Utils
  ( mkFinalAst,
    liftEither500,
  )
where

import Control.Exception (Exception (displayException))
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Generics.Labels ()
import Inferno.Core (Interpreter (Interpreter, parseAndInferTypeReps))
import Inferno.ML.Remote.Types
  ( RemoteM,
    Script (Script),
    SomeInfernoError (SomeInfernoError),
  )
import Inferno.ML.Types.Value (MlValue)
import Inferno.Types.Syntax
  ( Expr,
    SourcePos,
  )
import Inferno.Types.VersionControl (VCObjectHash)
import Servant (ServerError (errBody), err500)

mkFinalAst ::
  Interpreter MlValue ->
  Script ->
  Either
    SomeInfernoError
    (Expr (Maybe VCObjectHash) SourcePos)
mkFinalAst Interpreter {parseAndInferTypeReps} (Script src) =
  first SomeInfernoError . parseAndInferTypeReps $ src

liftEither500 :: forall e a. Exception e => Either e a -> RemoteM a
liftEither500 = either (throwM . mk500) pure
  where
    mk500 :: Show e => e -> ServerError
    mk500 (ByteString.Lazy.Char8.pack . displayException -> e) =
      err500
        { errBody = "Script evalution failed with: " <> e
        }
