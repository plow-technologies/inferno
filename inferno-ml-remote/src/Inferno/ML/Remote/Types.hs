{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Inferno.ML.Remote.Types
  ( InfernoMlRemoteAPI,
    Options (Options),
    Script (Script),
    EvalResult (EvalResult),
    InfernoMlRemoteM,
    InfernoMlRemoteEnv (InfernoMlRemoteEnv),
    ModelStore (..),
    ModelStoreOption (..),
    ModelCache (ModelCache),
    SomeInfernoError (..),
    InfernoMlRemoteError (..),
    UserId (UserId),
    ModelName (ModelName),
    ModelRow (ModelRow),
    parseOptions,
    mkOptions,
  )
where

import Control.Applicative (asum, (<**>))
import Control.Exception (Exception (displayException), throwIO)
import Control.Monad.Reader (ReaderT)
import Data.Aeson
  ( FromJSON (parseJSON),
    Object,
    ToJSON,
    eitherDecodeFileStrict,
    withObject,
    (.:),
  )
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Database.PostgreSQL.Simple
  ( ConnectInfo (ConnectInfo),
    Connection,
    FromRow,
    ToRow,
  )
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.Generics (Generic)
import qualified Options.Applicative as Options
import Servant (Handler, JSON, Post, ReqBody, (:>))

type InfernoMlRemoteAPI =
  "inference" :> ReqBody '[JSON] Script :> Post '[JSON] EvalResult

type InfernoMlRemoteM = ReaderT InfernoMlRemoteEnv Handler

data InfernoMlRemoteEnv = InfernoMlRemoteEnv
  { modelCache :: ModelCache,
    modelStore :: ModelStore
  }
  deriving stock (Generic)

-- | The actual model store itself, not the 'ModelStoreOption', which only specifies
-- how the store should be connected to. For the 'Paths' option, this is simple, but
-- for other backends there will need to be initialization, thus the two separate
-- types
data ModelStore
  = -- | Path to source directory holding models
    Paths FilePath
  | Postgres Connection
  deriving stock (Eq, Generic)

-- | Config for caching ML models to be used with Inferno scripts. When a script
-- uses @ML.loadModel@, models will be copied from the source configured in
-- 'ModelCacheOption' and saved to the 'cache' directory. Once the `maxSize` has
-- been exceeded, least-recently-used cached models will be removed
data ModelCache = ModelCache
  { -- | Directory where the models should be cached
    path :: FilePath,
    -- | Maximum size in bytes of the model cache directory
    maxSize :: Word64
  }
  deriving stock (Show, Eq, Generic)

newtype ModelName = ModelName Text
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromField, ToField)

data ModelRow = ModelRow
  { name :: ModelName,
    model :: ByteString,
    -- Storing the size when creating the model row helps avoid needing to
    -- compure it later on, to ensure that adding the model doesn't cause the
    -- cache to exceed it's maximum size. Since the models can be large it's
    -- better to avoid needing to calculate the size
    size :: Int64,
    user :: Maybe UserId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromRow, ToRow)

newtype UserId = UserId Text
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromField, ToField)

-- | Generic container for errors that may arise when parsing\/typechecking
-- Inferno scripts in handlers. It doesn\'t matter what the specific error is
-- as it will only be used in a 400 or 500 response with a message body containing
-- the error
data SomeInfernoError where
  SomeInfernoError :: forall a. Show a => a -> SomeInfernoError

deriving stock instance Show SomeInfernoError

instance Exception SomeInfernoError where
  displayException (SomeInfernoError x) = show x

-- TODO
-- Add more ways to load?
data ModelStoreOption
  = -- | Path to source directory holding models
    PathOption FilePath
  | PostgresOption ConnectInfo
  deriving stock (Show, Eq, Generic)

instance FromJSON ModelStoreOption where
  parseJSON = withObject "ModelCacheOption" $ \o ->
    asum
      [ PathOption <$> o .: "path",
        fmap PostgresOption $ connInfoP =<< o .: "postgres"
      ]
    where
      connInfoP :: Object -> Parser ConnectInfo
      connInfoP o =
        ConnectInfo
          <$> o
          .: "host"
          <*> o
          .: "port"
          <*> o
          .: "user"
          <*> o
          .: "password"
          <*> o
          .: "database"

instance FromJSON ModelCache where
  parseJSON = withObject "ModelCache" $ \o ->
    ModelCache <$> o .: "path" <*> o .: "max-size"

data Options = Options
  { port :: Word64,
    modelCache :: ModelCache,
    modelStore :: ModelStoreOption
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Options where
  parseJSON = withObject "Options" $ \o ->
    Options <$> o .: "port" <*> o .: "model-cache" <*> o .: "model-store"

newtype Script = Script Text
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON, IsString)

newtype EvalResult = EvalResult Text
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON, IsString)

data InfernoMlRemoteError
  = CacheSizeExceeded
  | NoSuchModel ModelName
  | ExternalProcessFailed FilePath Int
  deriving stock (Show, Eq, Generic)

instance Exception InfernoMlRemoteError where
  displayException = \case
    CacheSizeExceeded -> "Model exceeds maximum cache size"
    ExternalProcessFailed p ec ->
      unwords
        [ "Process",
          "'" <> p <> "'",
          "failed with exit code",
          show ec
        ]
    NoSuchModel (ModelName m) ->
      unwords
        [ "Model:",
          "'" <> Text.unpack m <> "'",
          "does not exist in the store"
        ]

mkOptions :: IO Options
mkOptions =
  either (throwIO . userError) pure
    =<< eitherDecodeFileStrict @Options
    =<< parseOptions

parseOptions :: IO FilePath
parseOptions = Options.execParser opts
  where
    opts :: Options.ParserInfo FilePath
    opts =
      Options.info (cfgFileP <**> Options.helper) $
        Options.fullDesc <> Options.progDesc "Server for `inferno-ml-remote`"

    cfgFileP :: Options.Parser FilePath
    cfgFileP = Options.strOption $ Options.long "config" <> Options.metavar "FILEPATH"
