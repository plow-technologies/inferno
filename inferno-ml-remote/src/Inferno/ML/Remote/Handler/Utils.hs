{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Inferno.ML.Remote.Handler.Utils
  ( mkFinalAst,
    liftEither500,
    firstOrThrow,
    queryStore,
  )
where

import Control.Exception (Exception (displayException))
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Generics.Labels ()
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (FromRow, Query, ToRow, query)
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
import Lens.Micro.Platform (view)
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

queryStore :: (ToRow b, FromRow a) => Query -> b -> RemoteM [a]
queryStore q x = asks (view #store) >>= \conn -> liftIO $ query conn q x

firstOrThrow :: (MonadThrow m, Exception e) => e -> [a] -> m a
firstOrThrow e = maybe (throwM e) pure . listToMaybe
