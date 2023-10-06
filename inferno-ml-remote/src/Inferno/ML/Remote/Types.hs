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
    RemoteM,
    Env (Env),
    Options (Options),
    Script (Script),
    InferenceResponse (InferenceResponse),
    ModelCache (ModelCache),
    Id (Id),
    ModelName (ModelName),
    Model (Model),
    User (User),
    InferenceParam (InferenceParam),
    InferenceRequest (InferenceRequest),
    RequestedModel (RequestedModel),
    SomeInfernoError (..),
    RemoteError (..),
    parseOptions,
    mkOptions,
  )
where

import Control.Applicative ((<**>))
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
import Database.PostgreSQL.Simple.LargeObjects (Oid)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.Generics (Generic)
import qualified Options.Applicative as Options
import Servant (JSON, Post, ReqBody, (:>))

type InfernoMlRemoteAPI =
  "inference" :> ReqBody '[JSON] InferenceRequest :> Post '[JSON] InferenceResponse

type RemoteM = ReaderT Env IO

data Env = Env
  { cache :: ModelCache,
    store :: Connection
  }
  deriving stock (Generic)

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
  deriving newtype
    ( Eq,
      FromField,
      ToField,
      IsString,
      FromJSON,
      ToJSON
    )

data Model = Model
  { id :: Id Model,
    name :: ModelName,
    -- The actual contents of the model
    contents :: Oid,
    version :: Text,
    -- Not currently used
    user :: Maybe User
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromRow, ToRow)

data InferenceParam = InferenceParam
  { id :: Id InferenceParam,
    -- FIXME Better type
    script :: Script,
    user :: User
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromRow, ToRow)

newtype Id a = Id Int64
  deriving stock (Show, Generic)
  deriving newtype
    ( Eq,
      FromField,
      ToField,
      FromJSON,
      ToJSON
    )

newtype User = User Text
  deriving stock (Show, Generic)
  deriving newtype
    ( Eq,
      FromField,
      ToField,
      FromJSON,
      ToJSON
    )

instance FromJSON ModelCache where
  parseJSON = withObject "ModelCache" $ \o ->
    ModelCache <$> o .: "path" <*> o .: "max-size"

data InferenceRequest = InferenceRequest
  { parameter :: Id InferenceParam,
    user :: User,
    model :: RequestedModel
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data RequestedModel = RequestedModel
  { name :: ModelName,
    version :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype Script = Script Text
  deriving stock (Show, Generic)
  deriving newtype
    ( Eq,
      FromJSON,
      ToJSON,
      IsString,
      FromField,
      ToField
    )

-- FIXME
-- The endpoint should return something structured
newtype InferenceResponse = InferenceResponse Text
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON, IsString)

data Options = Options
  { port :: Word64,
    cache :: ModelCache,
    store :: ConnectInfo
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Options where
  parseJSON = withObject "Options" $ \o ->
    Options <$> o .: "port" <*> o .: "cache" <*> (connInfoP =<< o .: "store")
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

data RemoteError
  = CacheSizeExceeded
  | NoSuchModel ModelName
  | NoSuchParameter (Id InferenceParam)
  | OtherError String
  deriving stock (Show, Eq, Generic)

instance Exception RemoteError where
  displayException = \case
    CacheSizeExceeded -> "Model exceeds maximum cache size"
    NoSuchModel (ModelName m) ->
      unwords
        [ "Model:",
          "'" <> Text.unpack m <> "'",
          "does not exist in the store"
        ]
    NoSuchParameter iid ->
      unwords ["Parameter:", "'" <> show iid <> "'", "does not exist"]
    OtherError e -> e

data SomeInfernoError where
  SomeInfernoError :: forall a. Show a => a -> SomeInfernoError

deriving stock instance Show SomeInfernoError

instance Exception SomeInfernoError where
  displayException (SomeInfernoError x) = show x
