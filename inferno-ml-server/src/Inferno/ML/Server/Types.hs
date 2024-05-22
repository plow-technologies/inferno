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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Inferno.ML.Server.Types
  ( module Inferno.ML.Server.Types,
    module M,
  )
where

import Control.Applicative (asum, (<**>))
import Control.Exception (Exception (displayException))
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
    (.:),
  )
import Data.Aeson.Types (Parser)
import qualified Data.Bits as Bits
import qualified Data.Bson as Bson
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Data (Typeable)
import Data.Generics.Labels ()
import Data.Generics.Wrapped (wrappedTo)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Word (Word64)
import Data.Yaml (decodeFileThrow)
import Database.PostgreSQL.Simple
  ( ConnectInfo (ConnectInfo),
    Connection,
    ResultError (ConversionFailed, UnexpectedNull),
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
import "inferno-ml-server-types" Inferno.ML.Server.Types as M hiding
  ( EvaluationInfo,
    InferenceParam,
    InferenceScript,
    InfernoMlServerAPI,
    Model,
    ModelVersion,
  )
import qualified "inferno-ml-server-types" Inferno.ML.Server.Types as Types
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
import Servant.Client.Streaming (ClientError)
import System.Posix.Types (EpochTime)
import Text.Read (readMaybe)
import UnliftIO (Async)
import UnliftIO.IORef (IORef)
import UnliftIO.MVar (MVar)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

type RemoteM = ReaderT Env IO

data Env = Env
  { config :: Config,
    store :: Connection,
    tracer :: IOTracer RemoteTrace,
    -- Lock for starting inference evaluation
    lock :: MVar (),
    -- The current inference evaluation job, if any
    job ::
      MVar
        ( -- ID for the inference param
          Id InferenceParam,
          -- The actual job itself. This is stored so it can be canceled later
          Async (Maybe (Types.WriteStream IO))
        ),
    bridge :: Bridge,
    manager :: Manager,
    -- The interpreter needs to be updated if the bridge info changes,
    -- hence the need to keep it in an `IORef`
    interpreter :: IORef (Maybe (Interpreter RemoteM BridgeMlValue))
  }
  deriving stock (Generic)

-- | A bridge (host and port) of a server that can proxy a data source. This
-- can be set using @POST /bridge@. It's not included directly into the NixOS
-- image because then it would be difficult to change
newtype Bridge = Bridge
  { info :: IORef (Maybe BridgeInfo)
  }
  deriving stock (Generic)

-- | Config for caching ML models to be used with Inferno scripts. When a script
-- uses @ML.loadModel@, models will be copied from the DB and saved to the cache
-- directory. Once the 'maxSize' has been exceeded, least-recently-used cached
-- models will be removed
data ModelCache = ModelCache
  { -- | Directory where the models should be cached
    path :: FilePath,
    -- | Maximum size in bytes of the model cache directory
    maxSize :: Word64
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
        . Text.drop 1

instance ToJSON (EntityId a) where
  toJSON = String . Text.pack . ('o' :) . entityIdToHex

instance Typeable a => FromField (EntityId a) where
  fromField f =
    maybe (returnError UnexpectedNull f mempty) $
      maybe (returnError ConversionFailed f mempty) (pure . entityIdFromInteger)
        . readMaybe @Integer
        . ByteString.Char8.unpack

instance ToField (EntityId a) where
  toField o = toField $ case readHex @Integer (entityIdToHex o) of
    (n, _) : _ -> n
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
  { port :: Word64,
    cache :: ModelCache,
    -- | Timeout for script evaluation
    --
    -- NOTE: Timeout is in seconds, not milliseconds
    timeout :: Word64,
    -- | Configuration for PostgreSQL database
    store :: ConnectInfo
  }
  deriving stock (Show, Eq, Generic)

{- ORMOLU_DISABLE -}
instance FromJSON Config where
  parseJSON = withObject "Config" $ \o ->
    Config
      <$> o .: "port"
      <*> o .: "cache"
      <*> o .: "timeout"
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
  { author :: EntityId UId,
    scriptTypes :: [Text],
    categoryIds :: [Int]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance FromJSON ScriptMetadata where
  parseJSON x =
    asum
      [ genericParseJSON defaultOptions x,
        -- In this case, only the user ID is provided
        fromUid <$> parseJSON @(EntityId UId) x
      ]
    where
      fromUid :: EntityId UId -> ScriptMetadata
      fromUid uid = ScriptMetadata uid mempty mempty

data RemoteError
  = CacheSizeExceeded
  | -- | Either the requested model version does not exist, or the
    -- parent model row corresponding to the model version does not
    -- exist
    NoSuchModel Int64
  | NoSuchScript VCObjectHash
  | NoSuchParameter Int64
  | InvalidScript Text
  | InvalidOutput Text
  | -- | Any error condition returned by Inferno script evaluation
    InfernoError SomeInfernoError
  | BridgeNotRegistered
  | ScriptTimeout Int
  | ClientError ClientError
  | OtherRemoteError Text
  deriving stock (Show, Eq, Generic)

instance Exception RemoteError where
  displayException = \case
    CacheSizeExceeded -> "Model exceeds maximum cache size"
    NoSuchModel m ->
      unwords
        [ "Model:",
          "'" <> show m <> "'",
          "does not exist in the store"
        ]
    NoSuchScript vch ->
      unwords
        [ "Script identified by hash",
          show vch,
          "does not exist"
        ]
    NoSuchParameter iid ->
      unwords ["Parameter:", "'" <> show iid <> "'", "does not exist"]
    InvalidScript t -> Text.unpack t
    InvalidOutput t ->
      unwords
        [ "Script output should be an array of `write` but was",
          Text.unpack t
        ]
    InfernoError (SomeInfernoError x) ->
      unwords
        [ "Inferno evaluation failed with:",
          show x
        ]
    BridgeNotRegistered -> "No bridge has been registered"
    ScriptTimeout t ->
      unwords
        [ "Script evaluation timed out after",
          show $ t `div` 1000000,
          "seconds"
        ]
    ClientError ce ->
      unwords
        [ "Client error:",
          displayException ce
        ]
    OtherRemoteError e -> Text.unpack e

data SomeInfernoError where
  SomeInfernoError :: forall a. Show a => a -> SomeInfernoError

deriving stock instance Show SomeInfernoError

instance Eq SomeInfernoError where
  _ == _ = False

instance Exception SomeInfernoError where
  displayException (SomeInfernoError x) = show x

data RemoteTrace
  = StartingServer
  | RemoteError RemoteError
  | OtherInfo Text
  | OtherWarn Text
  | RegisteringBridge BridgeInfo
  | RunningInference (Id InferenceParam) Int
  | EvaluatingScript (Id InferenceParam)
  | CopyingModel (Id ModelVersion)
  | CancelingInference (Id InferenceParam)
  deriving stock (Show, Eq, Generic)

logTrace :: RemoteTrace -> RemoteM ()
logTrace t = (`traceWith` t) =<< view #tracer

-- FLAP!
infixl 4 ??

(??) :: Functor f => f (a -> b) -> a -> f b
f ?? x = ($ x) <$> f

type InferenceParam =
  Types.InferenceParam (EntityId UId) (EntityId GId) PID VCObjectHash

type EvaluationInfo = Types.EvaluationInfo (EntityId UId) (EntityId GId) PID

type Model = Types.Model (EntityId UId) (EntityId GId)

type ModelVersion = Types.ModelVersion (EntityId UId) (EntityId GId) Oid

type InferenceScript = Types.InferenceScript ScriptMetadata (EntityId GId)

type VCMeta a = Inferno.VersionControl.Types.VCMeta ScriptMetadata (EntityId GId) a

pattern InferenceScript :: VCObjectHash -> VCMeta VCObject -> InferenceScript
pattern InferenceScript h o = Types.InferenceScript h o

pattern InferenceParam ::
  Maybe (Id InferenceParam) ->
  VCObjectHash ->
  Id ModelVersion ->
  Vector (SingleOrMany PID) ->
  Vector (SingleOrMany PID) ->
  Maybe UTCTime ->
  EntityId UId ->
  InferenceParam
pattern InferenceParam iid s m is os mt uid =
  Types.InferenceParam iid s m is os mt uid

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

type InfernoMlServerAPI =
  Types.InfernoMlServerAPI (EntityId UId) (EntityId GId) PID VCObjectHash EpochTime

-- Orphans

instance FromField VCObjectHash where
  fromField f mb = wrappedTo <$> fromField @VCObjectHashRow f mb

instance ToField VCObjectHash where
  toField = toField . VCObjectHashRow

deriving newtype instance ToHttpApiData EpochTime

deriving newtype instance FromHttpApiData EpochTime
