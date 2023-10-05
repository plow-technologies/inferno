{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Inferno.ML.Remote.Utils
  ( mkFinalAst,
    cacheAndUseModel,
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
    RemoteM,
    Model,
    ModelCache,
    ModelName (ModelName),
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
import UnliftIO (MonadUnliftIO)
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
cacheAndUseModel :: ModelName -> RemoteM ()
cacheAndUseModel (ModelName _) = undefined

modelsByAccessTime :: forall m. MonadIO m => FilePath -> m [FilePath]
modelsByAccessTime = sortByM compareAccessTime <=< listDirectory
  where
    compareAccessTime :: FilePath -> FilePath -> m Ordering
    compareAccessTime f1 f2 =
      getAccessTime f1 >>= \t1 ->
        compare t1 <$> getAccessTime f2
