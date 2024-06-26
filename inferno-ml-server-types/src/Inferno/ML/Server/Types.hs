{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Inferno.ML.Server.Types where

import Conduit (ConduitT)
import Control.Applicative (asum, optional)
import Control.Category ((>>>))
import Control.DeepSeq (NFData (rnf), rwhnf)
import Control.Monad (void, (<=<))
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Data (Typeable)
import Data.Generics.Product (HasType (typed), the)
import Data.Generics.Wrapped (wrappedTo)
import Data.Hashable (Hashable)
import qualified Data.IP
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word32, Word64)
import Database.PostgreSQL.Simple.FromField
  ( Conversion,
    Field,
    FromField (fromField),
    ResultError (ConversionFailed, UnexpectedNull),
    returnError,
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.LargeObjects (Oid (Oid))
import Database.PostgreSQL.Simple.Newtypes (Aeson (Aeson), getAeson)
import Database.PostgreSQL.Simple.ToField
  ( Action (Escape, EscapeByteA),
    ToField (toField),
  )
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
import Database.PostgreSQL.Simple.Types
  ( Binary (Binary),
    Default (Default),
  )
import Foreign.C (CUInt (CUInt))
import GHC.Generics (Generic)
import Inferno.Types.Syntax (Ident)
import Inferno.Types.VersionControl
  ( VCObjectHash,
    byteStringToVCObjectHash,
    vcObjectHashToByteString,
  )
import Inferno.VersionControl.Types (VCMeta, VCObject)
import Lens.Micro.Platform hiding ((.=))
import Servant
  ( Capture,
    Get,
    JSON,
    NewlineFraming,
    Put,
    QueryParam,
    QueryParam',
    Required,
    StreamPost,
    (:<|>),
    (:>),
  )
import Servant.Conduit ()
import System.Posix (EpochTime)
import Text.Read (readMaybe)
import URI.ByteString (Absolute, URIRef)
import URI.ByteString.Aeson ()
import Web.HttpApiData
  ( FromHttpApiData (parseUrlPiece),
    ToHttpApiData (toUrlPiece),
  )

-- API type for `inferno-ml-server`
type InfernoMlServerAPI uid gid p s t =
  -- Check if the server is up and if any job is currently running:
  --
  --  * `Nothing` -> The server is evaluating a script
  --  * `Just ()` -> The server is not doing anything and can be killed
  --
  -- This can be implemented using an `MVar ()`
  "status" :> Get '[JSON] (Maybe ())
    -- Evaluate an inference script
    :<|> "inference"
      :> Capture "id" (Id (InferenceParam uid gid p s))
      :> QueryParam "res" Int64
      :> QueryParam' '[Required] "uuid" UUID
      :> StreamPost NewlineFraming JSON (WriteStream IO)
    :<|> "inference" :> "cancel" :> Put '[JSON] ()

-- A bridge to get or write data for use with Inferno scripts. This is implemented
-- by a bridge server connected to a data source, not by `inferno-ml-server`
type BridgeAPI p t =
  "bridge"
    :> "value-at"
    :> QueryParam' '[Required] "res" Int64
    :> QueryParam' '[Required] "p" p
    :> QueryParam' '[Required] "time" t
    :> Get '[JSON] IValue
    :<|> "bridge"
      :> "latest-value-and-time-before"
      :> QueryParam' '[Required] "time" t
      :> QueryParam' '[Required] "p" p
      :> Get '[JSON] IValue
    :<|> "bridge"
      :> "values-between"
      :> QueryParam' '[Required] "res" Int64
      :> QueryParam' '[Required] "p" p
      :> QueryParam' '[Required] "t1" t
      :> QueryParam' '[Required] "t2" t
      :> Get '[JSON] IValue

-- | Stream of writes that an ML parameter script results in. Each element
-- in the stream is a chunk (sub-list) of the original values that the
-- inference script evaluates to. For example, given the following output:
-- @[ (1, [ (100, 5.0) .. (10000, 5000.0) ]) ]@; the stream items will be:
-- @(1, [ (100, 5.0) .. (500, 2500.0) ]), (1, [ (501, 2501.0) .. (10000, 5000.0) ])@.
-- This means the same output may appear more than once in the stream
type WriteStream m = ConduitT () (Int, [(EpochTime, IValue)]) m ()

-- | Information for contacting a bridge server that implements the 'BridgeAPI'
data BridgeInfo uid gid p s = BridgeInfo
  { id :: Id (InferenceParam uid gid p s),
    host :: IPv4,
    port :: Word64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

instance FromRow (BridgeInfo uid gid p s) where
  fromRow =
    BridgeInfo
      <$> field
      <*> field
      <*> fmap (fromIntegral @Int64) field

instance ToRow (BridgeInfo uid gid p s) where
  toRow bi =
    [ bi ^. the @"id" & toField,
      bi ^. the @"host" & toField,
      bi ^. the @"port" & toField
    ]

-- | The ID of a database entity
newtype Id a = Id Int64
  deriving stock (Show, Generic)
  deriving newtype
    ( Eq,
      Ord,
      Hashable,
      FromField,
      ToField,
      FromJSON,
      ToJSON,
      FromJSONKey,
      ToJSONKey,
      ToHttpApiData,
      FromHttpApiData
    )
  deriving anyclass (NFData)

-- | Row for the table containing inference script closures
data InferenceScript uid gid = InferenceScript
  { -- | This is the ID for each row, stored as a @bytea@ (bytes of the hash)
    hash :: VCObjectHash,
    -- | Script closure
    obj :: VCMeta uid gid VCObject
  }
  deriving stock (Show, Eq, Generic)

-- Newtype just for `FromRow`/`ToRow` instances. It would be possible to just
-- add the instances to `inferno-types`, but then there would be a dependency
-- on `postgresql-simple`
newtype VCObjectHashRow = VCObjectHashRow VCObjectHash
  deriving stock (Generic)

instance FromField VCObjectHashRow where
  fromField f = \case
    Nothing -> returnError UnexpectedNull f "Expected non-empty bytea"
    Just bs ->
      fromField @(Binary ByteString) f (Just bs) >>= \case
        Binary b
          | Just h <- byteStringToVCObjectHash b ->
              pure $ VCObjectHashRow h
        _ -> returnError ConversionFailed f "Invalid hash"

instance ToField VCObjectHashRow where
  toField = EscapeByteA . vcObjectHashToByteString . wrappedTo

instance (ToJSON uid, ToJSON gid) => ToRow (InferenceScript uid gid) where
  toRow s =
    -- NOTE: Don't change the order!
    [ s ^. the @"hash" & VCObjectHashRow & toField,
      s ^. the @"obj" & Aeson & toField
    ]

instance
  ( FromJSON uid,
    FromJSON gid,
    Typeable uid,
    Typeable gid
  ) =>
  FromRow (InferenceScript uid gid)
  where
  fromRow =
    InferenceScript
      <$> fmap wrappedTo (field @VCObjectHashRow)
      <*> fmap getAeson field

-- | Row of the model table, parameterized by the user and group type. This
-- table contains metadata for models that should not change between different
-- versions, e.g. model name and permissions. A second table, 'ModelVersion',
-- contains the specific versions of each model (and the actual model contents),
-- along with other metadata that may change between versions
data Model uid gid = Model
  { id :: Maybe (Id (Model uid gid)),
    name :: Text,
    -- | Permissions for reading or updating the model, keyed by the group ID
    -- type
    --
    -- NOTE: This is stored as a @jsonb@ rather than as @hstore@. It could
    -- currently be stored as an @hstore@, but later we might want to
    -- use a more complex type that we could not easily convert to\/from
    -- text (which is required to use @hstore@). So using @jsonb@ allows
    -- for greater potential flexibility
    permissions :: Map gid ModelPermissions,
    -- | The user who owns the model, if any. Note that owning a model
    -- will implicitly set permissions
    user :: Maybe uid,
    -- | The time that this model was \"deleted\", if any. For active models,
    -- this will be @Nothing@
    terminated :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance NFData (Model uid gid) where
  rnf = rwhnf

instance
  ( Typeable gid,
    FromField uid,
    FromField gid,
    FromJSONKey gid,
    Ord gid
  ) =>
  FromRow (Model uid gid)
  where
  -- NOTE: Order of fields must align exactly with DB schema
  fromRow =
    Model
      <$> field
      <*> field
      <*> fmap getAeson field
      <*> field
      <*> field

instance
  ( ToField uid,
    ToField gid,
    ToJSONKey gid
  ) =>
  ToRow (Model uid gid)
  where
  -- NOTE: Order of fields must align exactly with DB schema
  toRow m =
    [ toField Default,
      m ^. the @"name" & toField,
      m ^. the @"permissions" & Aeson & toField,
      m ^. the @"user" & toField,
      -- The `ToRow` instance is only for new rows, so we don't want
      -- to set the `terminated` field to anything by default
      --
      -- The same applies to the other `toField Default`s for different
      -- types below
      toField Default
    ]

{- ORMOLU_DISABLE -}
instance
  ( FromJSON uid,
    FromJSONKey gid,
    Ord gid
  ) =>
  FromJSON (Model uid gid)
  where
  parseJSON = withObject "Model" $ \o ->
    Model
      -- If a new model is being created, its ID will not be present
      <$> o .:? "id"
      <*> (ensureNotNull =<< o .: "name")
      <*> o .: "permissions"
      <*> o .:? "user"
      -- If a new model is being serialized, it does not really make
      -- sense to require a `"terminated": null` field
      <*> o .:? "terminated"
    where
      ensureNotNull :: Text -> Parser Text
      ensureNotNull
        t
          | Text.null t = fail "Field cannot be empty"
          | otherwise = pure t
{- ORMOLU_ENABLE -}

instance
  ( ToJSON uid,
    ToJSONKey gid
  ) =>
  ToJSON (Model uid gid)
  where
  toJSON m =
    object
      [ "id" .= view (the @"id") m,
        "name" .= view (the @"name") m,
        "permissions" .= view (the @"permissions") m,
        "user" .= view (the @"user") m,
        "terminated" .= view (the @"terminated") m
      ]

-- | Represents rows of the model version tables; each row is linked to its
-- 'Model' parent and also contains the actual contents of the model. This
-- is parameterized by the user and group types as well as the type of the
-- content, which will normally be an 'Oid' (Postgres large object). Other
-- model metadata is contained here as well, e.g. the model card, as this
-- might change between versions
data ModelVersion uid gid c = ModelVersion
  { id :: Maybe (Id (ModelVersion uid gid c)),
    -- | Foreign key of the @model@ table, which contains invariant metadata
    -- related to the model, i.e. name, permissions, user
    model :: Id (Model uid gid),
    card :: ModelCard,
    -- | The actual contents of version of the model. Normally this will be
    -- an 'Oid' pointing to the serialized bytes of the model imported into
    -- the PSQL large object table
    contents :: c,
    version :: Version,
    -- | The time that this model version was \"deleted\", if any. For active
    -- models versions, this will be @Nothing@
    terminated :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance
  ( FromField uid,
    FromField gid
  ) =>
  FromRow (ModelVersion uid gid Oid)
  where
  -- NOTE: Order of fields must align exactly with DB schema. This instance
  -- could just be `anyclass` derived but it's probably better to be as
  -- explicit as possible
  fromRow =
    ModelVersion
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance
  ( ToField uid,
    ToField gid
  ) =>
  ToRow (ModelVersion uid gid Oid)
  where
  -- NOTE: Order of fields must align exactly with DB schema
  toRow mv =
    [ toField Default,
      mv ^. the @"model" & toField,
      mv ^. the @"card" & Aeson & toField,
      mv ^. the @"contents" & toField,
      mv ^. the @"version" & toField,
      toField Default
    ]

{- ORMOLU_DISABLE -}
instance
  ( FromJSON uid,
    FromJSON gid
  ) =>
  FromJSON (ModelVersion uid gid Oid)
  where
  parseJSON = withObject "ModelVersion" $ \o ->
    ModelVersion
      -- Note that for a model serialized as JSON, the `id` must be present
      -- (this assumes that a model version serialized as JSON always refers
      -- to one that exists in the DB already)
      <$> fmap Just (o .: "id")
      <*> o .: "model"
      <*> o .: "card"
      <*> fmap (Oid . fromIntegral @Word64) (o .: "contents")
      <*> o .: "version"
      -- If a new model version is being serialized, it does not really make
      -- sense to require a `"terminated": null` field
      <*> o .:? "terminated"
{- ORMOLU_ENABLE -}

instance
  ( ToJSON uid,
    ToJSON gid
  ) =>
  ToJSON (ModelVersion uid gid Oid)
  where
  toJSON mv =
    object
      [ "id" .= view (the @"id") mv,
        "model" .= view (the @"model") mv,
        "contents" .= view (the @"contents" . to unOid) mv,
        "version" .= view (the @"version") mv,
        "card" .= view (the @"card") mv,
        "terminated" .= view (the @"terminated") mv
      ]
    where
      unOid :: Oid -> Word32
      unOid (Oid (CUInt x)) = x

-- | Permissions for reading or writing a model
data ModelPermissions
  = -- | The model can be read e.g. for inference
    ReadModel
  | -- | The model can be updated e.g. during training
    WriteModel
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance FromJSON ModelPermissions where
  parseJSON = withText "ModelPermissions" $ \case
    "read" -> pure ReadModel
    "write" -> pure WriteModel
    t -> fail $ unwords ["Invalid model permissions:", Text.unpack t]

instance ToJSON ModelPermissions where
  toJSON =
    String . \case
      ReadModel -> "read"
      WriteModel -> "write"

-- | Full description and metadata of the model
data ModelCard = ModelCard
  { -- | High-level, structured overview of model details and summary
    description :: ModelDescription,
    metadata :: ModelMetadata
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)
  deriving (FromField, ToField) via Aeson ModelCard

-- | Structured description of a model
data ModelDescription = ModelDescription
  { -- | General summary of model, cannot be empty
    summary :: Text,
    -- | How the model is intended to be used
    uses :: Text,
    -- | Applicable limitations, risks, biases, etc...
    risks :: Text,
    -- | Details on training data, speed\/size of training elements, etc...
    training :: Text,
    evaluation :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, NFData)

{- ORMOLU_DISABLE -}
instance FromJSON ModelDescription where
  parseJSON = withObject "ModelDescription" $ \o ->
    ModelDescription
      <$> o .: "summary"
      <*> o .:? "uses" .!= mempty
      <*> o .:? "risks" .!= mempty
      <*> o .:? "training" .!= mempty
      <*> o .:? "evaluation" .!= mempty
{- ORMOLU_ENABLE -}

-- | Metadata for the model, inspired by Hugging Face model card format
data ModelMetadata = ModelMetadata
  { languages :: Vector ISO63912,
    tags :: Vector Text,
    datasets :: Vector Text,
    metrics :: Vector Text,
    license :: Maybe Text,
    baseModel :: Maybe Text,
    thumbnail :: Maybe (Text, URIRef Absolute)
  }
  deriving stock (Show, Eq, Generic)

instance NFData ModelMetadata where
  rnf = rwhnf

instance FromJSON ModelMetadata where
  parseJSON = withObject "ModelMetadata" $ \o ->
    ModelMetadata
      <$> o .:? "languages" .!= mempty
      <*> o .:? "tags" .!= mempty
      <*> o .:? "datasets" .!= mempty
      <*> o .:? "metrics" .!= mempty
      <*> o .:? "license"
      <*> o .:? "base_model"
      <*> (thumbnailP =<< o .:? "thumbnail")
    where
      thumbnailP :: Maybe Object -> Parser (Maybe (Text, URIRef Absolute))
      thumbnailP = \case
        Nothing -> pure Nothing
        Just o -> fmap Just $ (,) <$> o .: "description" <*> o .: "url"

instance ToJSON ModelMetadata where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_',
          omitNothingFields = True
        }

-- | Similar to the @Version@ type from base, but allows for a leading @v@ and
-- guarantees that there is at least one digit. Digits must be separated by @.@;
-- multiple tags are allowed, separated by @-@
data Version
  = Version
      (NonEmpty Int)
      -- ^ List of digits for version string
      [Text]
      -- ^ Any tags
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- Compares based on digits, not on tag
instance Ord Version where
  compare = comparing . view $ typed @(NonEmpty Int)

instance FromJSON Version where
  parseJSON =
    withText "Version" $
      either fail pure
        . Attoparsec.parseOnly versionP
        . Text.Encoding.encodeUtf8

instance ToJSON Version where
  toJSON = String . showVersion

instance FromField Version where
  fromField f =
    maybe (returnError UnexpectedNull f mempty) $
      either (returnError ConversionFailed f) pure
        . Attoparsec.parseOnly versionP

instance ToField Version where
  toField = Escape . Text.Encoding.encodeUtf8 . showVersion

versionP :: Attoparsec.Parser Version
versionP = do
  void . optional $ Attoparsec.char 'v'
  Version <$> numbersP <*> tagsP
  where
    numbersP :: Attoparsec.Parser (NonEmpty Int)
    numbersP =
      -- Note that `sepBy1` will fail if there is not at least one occurrence,
      -- so using `fromList` is OK here
      fmap NonEmpty.fromList
        -- Note that if `many1` succeeds, `read` must also succeed
        . Attoparsec.sepBy1 (read <$> Attoparsec.many1 Attoparsec.digit)
        $ Attoparsec.char '.'

    tagsP :: Attoparsec.Parser [Text]
    tagsP =
      fmap (filter (not . Text.null) . fmap Text.Encoding.decodeUtf8)
        . Attoparsec.sepBy
          ( Attoparsec.takeWhile (Attoparsec.inClass "a-zA-Z")
          )
        $ Attoparsec.char '-'

showVersion :: Version -> Text
showVersion (Version ns ts) =
  mconcat
    [ "v",
      Text.intercalate "." . fmap tshow $ NonEmpty.toList ns,
      bool ("-" <> Text.intercalate "-" ts) mempty $ null ts
    ]

-- | Row of the inference parameter table, parameterized by the user, group, and
-- script type
data InferenceParam uid gid p s = InferenceParam
  { id :: Maybe (Id (InferenceParam uid gid p s)),
    -- | The script of the parameter
    --
    -- For new parameters, this will be textual or some other identifier
    -- (e.g. a UUID for use with @inferno-lsp@)
    --
    -- For existing inference params, this is the foreign key for the specific
    -- script in the 'InferenceScript' table (i.e. a @VCObjectHash@)
    script :: s,
    -- | All of the (specific versions of) models that can be used with this
    -- parameters. @inferno-ml-server@ will copy the contents of each of the
    -- model versions when evaluating inference scripts. Inference scripts can
    -- reference any of the linked model versions by referring to the parent
    -- model\'s name, e.g. @loadModel "name.ts.pt"@
    --
    -- Each element represents the ID of a specific model version. However, due
    -- to limitations in PostgreSQL, there is no referential integrity; i.e.
    -- the elements are treated as plain integers
    models :: Vector (Id (ModelVersion uid gid Oid)),
    -- | This is called @inputs@ but is also used for script outputs as
    -- well. The access (input or output) is controlled by the 'ScriptInputType'.
    -- For example, if this field is set to @[("input0", Single (p, Readable))]@,
    -- the script will only have a single read-only input and will not be able to
    -- write anywhere (note that we should disallow this scenario, as script
    -- evaluation would not work properly)
    --
    -- Mapping the input\/output to the Inferno identifier helps ensure that
    -- Inferno identifiers are always pointing to the correct input\/output;
    -- otherwise we would need to rely on the order of the original identifiers
    inputs :: Map Ident (SingleOrMany p, ScriptInputType),
    -- | Resolution, passed to bridge routes
    resolution :: Word64,
    -- | The time that this parameter was \"deleted\", if any. For active
    -- parameters, this will be @Nothing@
    terminated :: Maybe UTCTime,
    user :: uid
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, ToJSON)

{- ORMOLU_DISABLE -}
instance
  ( FromJSON s,
    FromJSON p,
    FromJSON uid
  ) =>
  FromJSON (InferenceParam uid gid p s)
  where
  parseJSON = withObject "InferenceParam" $ \o ->
    InferenceParam
      -- The ID needs to be included when deserializing
      <$> o .: "id"
      <*> o .: "script"
      <*> o .: "models"
      <*> o .: "inputs"
      <*> o .:? "resolution" .!= 128
      -- We shouldn't require this field
      <*> o .:? "terminated"
      <*> o .: "user"
{- ORMOLU_ENABLE -}

-- We only want this instance if the `script` is a `VCObjectHash` (because it
-- should not be possible to store a new param with a raw script)
instance
  ( FromJSON p,
    Typeable p,
    FromField uid,
    Typeable gid,
    Typeable uid
  ) =>
  FromRow (InferenceParam uid gid p VCObjectHash)
  where
  fromRow =
    InferenceParam
      <$> field
      <*> fmap wrappedTo (field @VCObjectHashRow)
      <*> field
      <*> fmap getAeson field
      <*> fmap fromIntegral (field @Int64)
      <*> field
      <*> field

instance
  ( ToJSON p,
    ToField uid
  ) =>
  ToRow (InferenceParam uid gid p VCObjectHash)
  where
  -- NOTE: Do not change the order of the field actions
  toRow ip =
    [ toField Default,
      ip ^. the @"script" & VCObjectHashRow & toField,
      ip ^. the @"models" & toField,
      ip ^. the @"inputs" & Aeson & toField,
      ip ^. the @"resolution" & Aeson & toField,
      toField Default,
      ip ^. the @"user" & toField
    ]

-- | Controls input interaction within a script, i.e. ability to read from
-- and\/or write to this input. Although the term \"input\" is used, those with
-- writes enabled can also be described as \"outputs\"
data ScriptInputType
  = -- | Script input can be read, but not written
    Readable
  | -- | Script input can be written, i.e. can be used in array of
    -- write objects returned from script evaluation
    Writable
  | -- | Script input can be both read from and written to; this allows
    -- the same script identifier to point to the same PID with both
    -- types of access enabled
    ReadableWritable
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance FromJSON ScriptInputType where
  parseJSON = withText "ScriptInputType" $ \case
    "r" -> pure Readable
    "w" -> pure Writable
    "rw" -> pure ReadableWritable
    s -> fail $ "Invalid script input type: " <> Text.unpack s

instance ToJSON ScriptInputType where
  toJSON =
    String . \case
      Readable -> "r"
      Writable -> "w"
      ReadableWritable -> "rw"

-- | Information about execution time and resource usage. This is saved by
-- @inferno-ml-server@ after script evaluation completes and can be queried
-- later by using the same job identifier that was provided to the @/inference@
-- route
data EvaluationInfo uid gid p = EvaluationInfo
  { -- | Note that this is the job identifier provided to the inference
    -- evaluation route, and is also the primary key of the database table
    id :: UUID,
    param :: Id (InferenceParam uid gid p VCObjectHash),
    -- | When inference evaluation started
    start :: UTCTime,
    -- | When inference evaluation ended
    end :: UTCTime,
    -- | The number of bytes allocated between the @start@ and @end@. Note
    -- that this is /total/ allocation over the course of evaluation, which
    -- can be many times greater than peak memory usage. Nevertheless, this
    -- can be useful to track memory usage over time and across different
    -- script revisions
    allocated :: Word64,
    -- | Additional CPU time used between the @start@ and @end@. This is
    -- converted from picoseconds to milliseconds
    cpu :: Word64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance FromRow (EvaluationInfo uid gid p) where
  fromRow =
    EvaluationInfo
      <$> field
      <*> field
      <*> field
      <*> field
      <*> fmap (fromIntegral @Int64) field
      <*> fmap (fromIntegral @Int64) field

instance ToRow (EvaluationInfo uid gid p) where
  toRow ei =
    [ ei ^. the @"id" & toField,
      ei ^. the @"param" & toField,
      ei ^. the @"start" & toField,
      ei ^. the @"end" & toField,
      ei ^. the @"allocated" & toField,
      ei ^. the @"cpu" & toField
    ]

-- | A user, parameterized by the user and group types
data User uid gid = User
  { id :: uid,
    groups :: Vector gid
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass
    ( FromRow,
      ToRow,
      FromJSON,
      ToJSON,
      NFData
    )

-- | IPv4 address with some useful instances
newtype IPv4 = IPv4 Data.IP.IPv4
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Read)

instance NFData IPv4 where
  rnf = rwhnf

instance FromJSON IPv4 where
  parseJSON =
    withText "IPv4" $
      maybe (fail "Invalid IPv4") (pure . IPv4)
        . readMaybe
        . Text.unpack

instance ToJSON IPv4 where
  toJSON = String . Text.pack . show

instance FromHttpApiData IPv4 where
  parseUrlPiece = maybe (Left "Invalid IPv4") Right . readMaybe . Text.unpack

instance ToHttpApiData IPv4 where
  toUrlPiece = Text.pack . show

instance FromField IPv4 where
  fromField = maybeConversion $ readMaybe . ByteString.Char8.unpack

instance ToField IPv4 where
  toField = Escape . ByteString.Char8.pack . show

toIPv4 :: (Int, Int, Int, Int) -> IPv4
toIPv4 (a, b, c, d) = IPv4 $ Data.IP.toIPv4 [a, b, c, d]

fromIPv4 :: IPv4 -> (Int, Int, Int, Int)
fromIPv4 =
  wrappedTo >>> Data.IP.fromIPv4 >>> \case
    [a, b, c, d] -> (a, b, c, d)
    -- Should not happen, `fromIPv4` always produces a 4-element list
    _ -> error "Invalid IP address"

-- Bridge-related types

-- | A value that can be used with Inferno and that will be consumed or returned
-- by @inferno-ml-server@ or the bridge server
--
-- Note that this is significantly more restrictive than Inferno's @Value@ type,
-- which cannot have sensible @ToJSON@ and @FromJSON@ instances
data IValue
  = IText Text
  | IDouble Double
  | ITuple (IValue, IValue)
  | ITime EpochTime
  | IEmpty
  | IArray (Vector IValue)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance FromJSON IValue where
  parseJSON = \case
    String t -> pure $ IText t
    Number n -> pure . IDouble $ toRealFloat n
    -- It's easier to just mark the time explicitly in an object,
    -- rather than try to deal with distinguishing times and doubles
    Object o ->
      asum
        [ ITime <$> o .: "time",
          fmap IArray $ arrayP =<< o .: "array"
        ]
    -- Note that this preserves a plain JSON array for tuples. But we need
    -- some straightforward way of distinguishing tuples and arrays; since
    -- the bridge often transmits a large number of individual tuples (times
    -- and values), it's better to use arrays for the tuples and a tagged object
    -- for arrays themselves; we often will only deal with one large array, and
    -- adding a few bytes to this is better than adding a few bytes to thousands
    -- of encoded tuples
    Array a
      | [x, y] <- Vector.toList a ->
          fmap ITuple $ (,) <$> parseJSON x <*> parseJSON y
      | otherwise -> fail "Only two-element tuples are supported"
    Null -> pure IEmpty
    _ -> fail "Expected one of: string, double, time, tuple, null, array"
    where
      arrayP :: Vector Value -> Parser (Vector IValue)
      arrayP a =
        -- This is a bit tedious, but we want to make sure that the array elements
        -- are homogeneous; parsing all elements to `IValue`s first can't guarantee
        -- this
        asum
          [ -- This alternative means that `null` will be correctly parsed to NaN
            -- when inside an array of doubles
            fmap IDouble <$> traverse parseJSON a,
            fmap ITuple <$> traverse parseJSON a,
            fmap IText <$> traverse parseJSON a,
            fmap ITime <$> traverse (withObject "EpochTime" (.: "time")) a,
            -- Nested array support
            fmap IArray
              <$> traverse (withObject "IArray" (arrayP <=< (.: "array"))) a,
            fail "Expected a heterogeneous array"
          ]

instance ToJSON IValue where
  toJSON = \case
    IDouble d -> toJSON d
    IText t -> toJSON t
    ITuple t -> toJSON t
    -- See `FromJSON` instance above
    ITime t -> object ["time" .= t]
    -- See `FromJSON` instance above
    IArray is -> object ["array" .= is]
    IEmpty -> toJSON Null

-- | Used to represent inputs to the script. 'Many' allows for an array input
data SingleOrMany a
  = Single a
  | Many (Vector a)
  deriving stock (Show, Eq, Generic, Functor)
  deriving anyclass (NFData)

instance FromJSON a => FromJSON (SingleOrMany a) where
  parseJSON v =
    asum
      [ Single <$> parseJSON v,
        Many <$> parseJSON v
      ]

instance ToJSON a => ToJSON (SingleOrMany a) where
  toJSON = \case
    Single a -> toJSON a
    Many as -> toJSON as

instance Ord a => Ord (SingleOrMany a) where
  compare a =
    (a,) >>> \case
      (Single x, Single y) -> compare x y
      (Many xs, Many ys) -> compare xs ys
      (Single _, Many _) -> LT
      (Many _, Single _) -> GT

tshow :: Show a => a -> Text
tshow = Text.pack . show

maybeConversion ::
  Typeable b => (a -> Maybe b) -> Field -> Maybe a -> Conversion b
maybeConversion f fld =
  maybe (returnError UnexpectedNull fld mempty) $
    maybe (returnError ConversionFailed fld mempty) pure
      . f

-- ISO63912 language tag for model card

data ISO63912 = ISO63912
  { code :: (Char, Char),
    name :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance FromJSON ISO63912 where
  parseJSON = withText "ISO63912" $ \case
    t
      | Just (f, r) <- Text.uncons t,
        Just (s, l) <- Text.uncons r,
        l == mempty ->
          maybe (fail "Missing ISO6391 language") pure $ fromChars (f, s)
      | otherwise -> fail "Invalid ISO63912 code"
    where
      fromChars :: (Char, Char) -> Maybe ISO63912
      fromChars c = ISO63912 c <$> Map.lookup c languagesByCode

instance ToJSON ISO63912 where
  toJSON (ISO63912 (f, s) _) = String . flip Text.snoc s $ Text.singleton f

{- ORMOLU_DISABLE -}
languagesByCode :: Map (Char, Char) Text
languagesByCode =
  Map.fromList
    [ (('a', 'a'), "Afar"), (('a', 'b'), "Abkhazian"), (('a', 'e'), "Avestan"),
      (('a', 'f'), "Afrikaans"), (('a', 'k'), "Akan"), (('a', 'm'), "Amharic"),
      (('a', 'n'), "Aragonese"), (('a', 'r'), "Arabic"), (('a', 's'), "Assamese"),
      (('a', 'v'), "Avaric"), (('a', 'y'), "Aymara"), (('a', 'z'), "Azerbaijani"),
      (('b', 'a'), "Bashkir"), (('b', 'e'), "Belarusian"), (('b', 'g'), "Bulgarian"),
      (('b', 'h'), "Bihari"), (('b', 'm'), "Bambara"), (('b', 'i'), "Bislama"),
      (('b', 'n'), "Bengali"), (('b', 'o'), "Tibetan"), (('b', 'r'), "Breton"),
      (('b', 's'), "Bosnian"), (('c', 'a'), "Catalan"), (('c', 'e'), "Chechen"),
      (('c', 'h'), "Chamorro"), (('c', 'o'), "Corsican"), (('c', 'r'), "Cree"),
      (('c', 's'), "Czech"), (('c', 'u'), "Church Slavic"), (('c', 'v'), "Chuvash"),
      (('c', 'y'), "Welsh"), (('d', 'a'), "Danish"), (('d', 'e'), "German"),
      (('d', 'v'), "Divehi"), (('d', 'z'), "Dzongkha"), (('e', 'e'), "Ewe"),
      (('e', 'l'), "Greek"), (('e', 'n'), "English"), (('e', 'o'), "Esperanto"),
      (('e', 's'), "Spanish"), (('e', 't'), "Estonian"), (('e', 'u'), "Basque"),
      (('f', 'a'), "Persian"), (('f', 'f'), "Fulah"), (('f', 'i'), "Finnish"),
      (('f', 'j'), "Fijian"), (('f', 'o'), "Faroese"), (('f', 'r'), "French"),
      (('f', 'y'), "Frisian"), (('g', 'a'), "Irish"), (('g', 'd'), "Gaelic"),
      (('g', 'l'), "Galician"), (('g', 'n'), "Guarani"), (('g', 'u'), "Gujarati"),
      (('g', 'v'), "Manx"), (('h', 'a'), "Hausa"), (('h', 'e'), "Hebrew"),
      (('h', 'i'), "Hindi"), (('h', 'o'), "Hiri Motu"), (('h', 'r'), "Croatian"),
      (('h', 't'), "Haitian"), (('h', 'u'), "Hungarian"), (('h', 'y'), "Armenian"),
      (('h', 'z'), "Herero"), (('i', 'a'), "Interlingua"), (('i', 'd'), "Indonesian"),
      (('i', 'e'), "Interlingue"), (('i', 'g'), "Igbo"), (('i', 'i'), "Sichuan Yi"),
      (('i', 'k'), "Inupiaq"), (('i', 'o'), "Ido"), (('i', 's'), "Icelandic"),
      (('i', 't'), "Italian"), (('i', 'u'), "Inuktitut"), (('j', 'a'), "Japanese"),
      (('j', 'v'), "Javanese"), (('k', 'a'), "Georgian"), (('k', 'g'), "Kongo"),
      (('k', 'i'), "Kikuyu"), (('k', 'j'), "Kuanyama"), (('k', 'k'), "Kazakh"),
      (('k', 'l'), "Kalaallisut"), (('k', 'm'), "Khmer"), (('k', 'n'), "Kannada"),
      (('k', 'o'), "Korean"), (('k', 'r'), "Kanuri"), (('k', 's'), "Kashmiri"),
      (('k', 'u'), "Kurdish"), (('k', 'v'), "Komi"), (('k', 'w'), "Cornish"),
      (('k', 'y'), "Kirghiz"), (('l', 'a'), "Latin"), (('l', 'b'), "Luxembourgish"),
      (('l', 'g'), "Ganda"), (('l', 'i'), "Limburgan"), (('l', 'n'), "Lingala"),
      (('l', 'o'), "Lao"), (('l', 't'), "Lithuanian"), (('l', 'u'), "Luba-Katanga"),
      (('l', 'v'), "Latvian"), (('m', 'g'), "Malagasy"), (('m', 'h'), "Marshallese"),
      (('m', 'i'), "Maori"), (('m', 'k'), "Macedonian"), (('m', 'l'), "Malayalam"),
      (('m', 'n'), "Mongolian"), (('m', 'r'), "Marathi"), (('m', 's'), "Malay"),
      (('m', 't'), "Maltese"), (('m', 'y'), "Burmese"), (('n', 'a'), "Nauru"),
      (('n', 'b'), "Bokmål"), (('n', 'd'), "Ndebele, North"), (('n', 'e'), "Nepali"),
      (('n', 'g'), "Ndonga"), (('n', 'l'), "Dutch"), (('n', 'n'), "Nynorsk"),
      (('n', 'o'), "Norwegian"), (('n', 'r'), "Ndebele"), (('n', 'v'), "Navajo"),
      (('n', 'y'), "Chichewa"), (('o', 'c'), "Occitan"), (('o', 'j'), "Ojibwa"),
      (('o', 'm'), "Oromo"), (('o', 'r'), "Oriya"), (('o', 's'), "Ossetian"),
      (('p', 'a'), "Panjabi"), (('p', 'i'), "Pali"), (('p', 'l'), "Polish"),
      (('p', 's'), "Pushto"), (('p', 't'), "Portuguese"), (('q', 'u'), "Quechua"),
      (('r', 'm'), "Romansh"), (('r', 'n'), "Rundi"), (('r', 'o'), "Romanian"),
      (('r', 'u'), "Russian"), (('r', 'w'), "Kinyarwanda"), (('s', 'a'), "Sanskrit"),
      (('s', 'c'), "Sardinian"), (('s', 'd'), "Sindhi"), (('s', 'e'), "Sami"),
      (('s', 'g'), "Sango"), (('s', 'i'), "Sinhala"), (('s', 'k'), "Slovak"),
      (('s', 'l'), "Slovenian"), (('s', 'm'), "Samoan"), (('s', 'n'), "Shona"),
      (('s', 'o'), "Somali"), (('s', 'q'), "Albanian"), (('s', 'r'), "Serbian"),
      (('s', 's'), "Swati"), (('s', 't'), "Sotho"), (('s', 'u'), "Sundanese"),
      (('s', 'v'), "Swedish"), (('s', 'w'), "Swahili"), (('t', 'a'), "Tamil"),
      (('t', 'e'), "Telugu"), (('t', 'g'), "Tajik"), (('t', 'h'), "Thai"),
      (('t', 'i'), "Tigrinya"), (('t', 'k'), "Turkmen"), (('t', 'l'), "Tagalog"),
      (('t', 'n'), "Tswana"), (('t', 'o'), "Tonga"), (('t', 'r'), "Turkish"),
      (('t', 's'), "Tsonga"), (('t', 't'), "Tatar"), (('t', 'w'), "Twi"),
      (('t', 'y'), "Tahitian"), (('u', 'g'), "Uighur"), (('u', 'k'), "Ukrainian"),
      (('u', 'r'), "Urdu"), (('u', 'z'), "Uzbek"), (('v', 'e'), "Venda"),
      (('v', 'i'), "Vietnamese"), (('v', 'o'), "Volapük"), (('w', 'a'), "Walloon"),
      (('w', 'o'), "Wolof"), (('x', 'h'), "Xhosa"), (('y', 'i'), "Yiddish"),
      (('y', 'o'), "Yoruba"), (('z', 'a'), "Zhuang"), (('z', 'h'), "Chinese"),
      (('z', 'u'), "Zulu")
    ]
{- ORMOLU_ENABLE -}
