{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Inferno.ML.Server.Types
  ( module Inferno.ML.Server.Types,
    module M,
  )
where

import Control.Applicative (Alternative ((<|>)), asum, (<**>))
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Data.Aeson
  ( FromJSON (parseJSON),
    FromJSONKey,
    Object,
    ToJSON (toJSON),
    ToJSONKey,
    Value (String),
    defaultOptions,
    genericParseJSON,
    withObject,
    withText,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Aeson.Types (Parser)
import qualified Data.Bits as Bits
import qualified Data.Bson as Bson
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Data (Typeable)
import Data.Generics.Labels ()
import Data.Generics.Wrapped (wrappedTo)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Word (Word64)
import Data.Yaml (decodeFileThrow)
import Database.PostgreSQL.Simple
  ( ConnectInfo (ConnectInfo),
    Connection,
    ResultError (ConversionFailed, UnexpectedNull),
    close,
    connect,
    (:.) ((:.)),
  )
import Database.PostgreSQL.Simple.FromField
  ( FromField (fromField),
    returnError,
  )
import Database.PostgreSQL.Simple.LargeObjects (Oid)
import Database.PostgreSQL.Simple.ToField
  ( ToField (toField),
  )
import Foreign.C (CTime (CTime))
import GHC.Generics (Generic)
import Inferno.Core (Interpreter)
import Inferno.ML.Server.Module.Types as M
import Inferno.VersionControl.Types
  ( VCObject,
    VCObjectHash,
    VCObjectPred,
    VCObjectVisibility,
  )
import qualified Inferno.VersionControl.Types
import Lens.Micro.Platform (view)
import Network.HTTP.Client (Manager)
import Numeric (readHex)
import qualified Options.Applicative as Options
import Plow.Logging (IOTracer, traceWith)
import Plow.Logging.Message
  ( LogLevel (LevelError, LevelInfo, LevelWarn),
  )
import System.Posix.Types (EpochTime)
import Text.Read (readMaybe)
import UnliftIO (Async, MonadUnliftIO)
import UnliftIO.Exception (bracket)
import UnliftIO.IORef (IORef)
import UnliftIO.MVar (MVar)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)
import "inferno-ml-server-types" Inferno.ML.Server.Types as M hiding
  ( BridgeInfo,
    EvaluationEnv,
    EvaluationInfo,
    InferenceParam,
    InferenceParamWithModels,
    InferenceScript,
    InfernoMlServerAPI,
    Model,
    ModelVersion,
    RemoteError,
    RemoteTrace,
  )
import qualified "inferno-ml-server-types" Inferno.ML.Server.Types as Types

type RemoteM = ReaderT Env IO

data Env = Env
  { config :: Config
  , tracer :: IOTracer RemoteTrace
  , store :: Pool Connection
  , -- Lock for starting inference evaluation
    lock :: MVar ()
  , -- The current inference evaluation job, if any
    job ::
      MVar
        ( -- ID for the inference param
          Id InferenceParam
        , -- The actual job itself. This is stored so it can be canceled later
          Async (Maybe (Types.WriteStream IO))
        )
  , manager :: Manager
  , -- The interpreter needs to be updated if the bridge info changes,
    -- hence the need to keep it in an `IORef`
    interpreter :: IORef (Maybe (Interpreter RemoteM BridgeMlValue))
  }
  deriving stock (Generic)

-- | Config for caching ML models to be used with Inferno scripts. When a script
-- uses @ML.loadModel@, models will be copied from the DB and saved to the cache
-- directory. Once the 'maxSize' has been exceeded, least-recently-used cached
-- models will be removed
data ModelCache = ModelCache
  { path :: FilePath
  -- ^ Directory where the models should be cached
  , maxSize :: Word64
  -- ^ Maximum size in bytes of the model cache directory
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ModelCache where
  parseJSON = withObject "ModelCache" $ \o ->
    ModelCache <$> o .: "path" <*> o .: "max-size"

-- The type for user and groups IDs. This is compatible with the `UserId` and
-- `GroupId` types from `all`, but we can't import those
newtype EntityId (a :: EntityIdType) = EntityId Bson.ObjectId
  deriving stock (Show, Generic, Typeable)
  deriving newtype (Eq, Ord)
  deriving anyclass (FromJSONKey, ToJSONKey)

instance FromJSON (EntityId a) where
  parseJSON =
    withText "EntityId" $
      fmap entityIdFromInteger
        . either fail (pure . fst)
        . Text.Read.hexadecimal
        -- Drop leading 'o'
        . Text.drop 1

instance ToJSON (EntityId a) where
  toJSON = String . Text.pack . ('o' :) . entityIdToHex

instance (Typeable a) => FromField (EntityId a) where
  fromField f =
    maybe (returnError UnexpectedNull f mempty) $
      maybe
        (returnError ConversionFailed f mempty)
        (pure . entityIdFromInteger . round)
        -- `numeric` column
        . readMaybe @Scientific
        . ByteString.Char8.unpack

instance ToField (EntityId a) where
  toField o = toField $ case readHex @Integer (entityIdToHex o) of
    (n, _) : _ -> fromInteger @Scientific n
    _ -> error "EntityId contained invalid fields"

entityIdFromInteger :: Integer -> EntityId a
entityIdFromInteger =
  fmap EntityId . Bson.Oid
    <$> fromInteger . (`Bits.shiftR` 64)
    <*> fromInteger

entityIdToHex :: EntityId a -> String
entityIdToHex (EntityId (Bson.Oid x y)) =
  Bson.showHexLen 8 x $
    Bson.showHexLen 16 y mempty

data EntityIdType
  = UId
  | GId
  deriving stock (Show, Eq, Generic, Typeable)

data Config = Config
  { port :: Word64
  , cache :: ModelCache
  , timeout :: Word64
  -- ^ Timeout for script evaluation
  --
  -- NOTE: Timeout is in seconds, not milliseconds
  , logLevel :: LogLevel
  -- ^ Minimum log level; logs below this level will be ignored
  , store :: ConnectInfo
  -- ^ Configuration for PostgreSQL database
  }
  deriving stock (Show, Eq, Generic)

{- ORMOLU_DISABLE -}
instance FromJSON Config where
  parseJSON = withObject "Config" $ \o ->
    Config
      <$> o .: "port"
      <*> o .: "cache"
      <*> o .: "timeout"
      <*> o .:? "log-level" .!= LevelWarn
      <*> (connInfoP =<< o .: "store")
    where
      connInfoP :: Object -> Parser ConnectInfo
      connInfoP o =
        ConnectInfo
          <$> o .: "host"
          <*> o .: "port"
          <*> o .: "user"
          <*> o .: "password"
          <*> o .: "database"
{- ORMOLU_ENABLE -}

mkOptions :: IO Config
mkOptions = decodeFileThrow =<< p
  where
    p :: IO FilePath
    p = Options.execParser opts
      where
        opts :: Options.ParserInfo FilePath
        opts =
          Options.info (cfgFileP <**> Options.helper) $
            Options.fullDesc
              <> Options.progDesc "Server for `inferno-ml-server`"

        cfgFileP :: Options.Parser FilePath
        cfgFileP =
          Options.strOption $
            Options.long "config"
              <> Options.metavar "FILEPATH"

-- | Metadata for Inferno scripts
data ScriptMetadata = ScriptMetadata
  { author :: EntityId UId
  , scriptTypes :: [ScriptType]
  , categoryIds :: [Int]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance FromJSON ScriptMetadata where
  parseJSON x =
    asum
      [ genericParseJSON defaultOptions x
      , -- In this case, only the user ID is provided
        fromUid <$> parseJSON @(EntityId UId) x
      ]
    where
      fromUid :: EntityId UId -> ScriptMetadata
      fromUid uid = ScriptMetadata uid mempty mempty

data ScriptType
  = MLInferenceScript InferenceOptions
  | OtherScript
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance FromJSON ScriptType where
  parseJSON v =
    genericParseJSON defaultOptions v
      <|> tagP v
      <|> pure OtherScript
    where
      tagP :: Value -> Parser ScriptType
      tagP = withText "ScriptType" $ \case
        "MLInferenceScript" ->
          pure . MLInferenceScript $
            InferenceOptions mempty
        _ -> pure OtherScript

newtype InferenceOptions = InferenceOptions
  { models :: Models (Id ModelVersion)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

logTrace :: RemoteTrace -> RemoteM ()
logTrace t =
  whenM shouldLog $
    (`traceWith` t) =<< view #tracer
  where
    shouldLog :: RemoteM Bool
    shouldLog = (traceLevel t >=) <$> view (#config . #logLevel)

logInfo :: TraceInfo InferenceParam ModelVersion -> RemoteM ()
logInfo = logTrace . InfoTrace

logWarn :: TraceWarn InferenceParam -> RemoteM ()
logWarn = logTrace . WarnTrace

logError :: RemoteError -> RemoteM ()
logError = logTrace . ErrorTrace

-- FLAP!
infixl 4 ??

(??) :: (Functor f) => f (a -> b) -> a -> f b
f ?? x = ($ x) <$> f

type RemoteError = Types.RemoteError InferenceParam ModelVersion

type RemoteTrace = Types.RemoteTrace InferenceParam ModelVersion

type InferenceParam = Types.InferenceParam (EntityId GId) PID

type InferenceParamWithModels = Types.InferenceParamWithModels (EntityId GId) PID

type BridgeInfo = Types.BridgeInfo (EntityId GId) PID

type EvaluationInfo = Types.EvaluationInfo (EntityId GId) PID

type Model = Types.Model (EntityId GId)

type ModelVersion = Types.ModelVersion (EntityId GId) Oid

type InferenceScript = Types.InferenceScript ScriptMetadata (EntityId GId)

type VCMeta a = Inferno.VersionControl.Types.VCMeta ScriptMetadata (EntityId GId) a

pattern InferenceScript :: VCObjectHash -> VCMeta VCObject -> InferenceScript
pattern InferenceScript h o = Types.InferenceScript h o

pattern InferenceParam ::
  Maybe (Id InferenceParam) ->
  VCObjectHash ->
  Inputs PID ->
  Outputs PID ->
  Word64 ->
  Maybe UTCTime ->
  EntityId GId ->
  InferenceParam
pattern InferenceParam iid s is os res mt gid =
  Types.InferenceParam iid s is os res mt gid

pattern InferenceParamWithModels ::
  InferenceParam -> Models (Id ModelVersion) -> InferenceParamWithModels
pattern InferenceParamWithModels ip mvs = Types.InferenceParamWithModels ip mvs

pattern BridgeInfo :: Id InferenceParam -> IPv4 -> Word64 -> BridgeInfo
pattern BridgeInfo ipid h p = Types.BridgeInfo ipid h p

pattern VCMeta ::
  CTime ->
  ScriptMetadata ->
  EntityId GId ->
  Text ->
  Text ->
  VCObjectPred ->
  VCObjectVisibility ->
  o ->
  VCMeta o
pattern VCMeta t a g n d p v o =
  Inferno.VersionControl.Types.VCMeta t a g n d p v o

pattern EvaluationInfo ::
  UUID ->
  Id InferenceParam ->
  UTCTime ->
  UTCTime ->
  Word64 ->
  Word64 ->
  EvaluationInfo
pattern EvaluationInfo u i s e m c = Types.EvaluationInfo u i s e m c

type InfernoMlServerAPI = Types.InfernoMlServerAPI (EntityId GId) PID

type EvaluationEnv = Types.EvaluationEnv (EntityId GId) PID

-- Orphans

instance FromField VCObjectHash where
  fromField f mb = wrappedTo <$> fromField @VCObjectHashRow f mb

instance ToField VCObjectHash where
  toField = toField . VCObjectHashRow

deriving newtype instance ToHttpApiData EpochTime

deriving newtype instance FromHttpApiData EpochTime

-- Etc

joinToTuple :: (a :. b) -> (a, b)
joinToTuple (a :. b) = (a, b)

traceLevel :: RemoteTrace -> LogLevel
traceLevel = \case
  InfoTrace{} -> LevelInfo
  WarnTrace{} -> LevelWarn
  ErrorTrace{} -> LevelError

-- | Create the connection pool for the DB
newConnectionPool :: ConnectInfo -> IO (Pool Connection)
#if MIN_VERSION_resource_pool(0,4,0)
newConnectionPool ci = Pool.newPool $ Pool.defaultPoolConfig (connect ci) close 60 10
#else
newConnectionPool ci = Pool.newPool $ Pool.PoolConfig (connect ci) close 60 10
#endif

withConnectionPool ::
  forall m a. (MonadUnliftIO m) => ConnectInfo -> (Pool Connection -> m a) -> m a
withConnectionPool = flip bracket destroyPool . liftIO . newConnectionPool
  where
    destroyPool :: Pool Connection -> m ()
    destroyPool = liftIO . Pool.destroyAllResources
