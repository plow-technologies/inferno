{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Inferno.ML.Server.Types where

import Conduit (ConduitT)
import Control.Applicative (asum, optional)
import Control.Category ((>>>))
import Control.DeepSeq (NFData (rnf), rwhnf)
import Control.Exception (Exception, displayException)
import Control.Monad (void)
import Crypto.Hash (digestFromByteString)
import Data.Aeson hiding (Value)
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as Base64.URL
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteUnits as ByteUnits
import Data.Char (chr)
import Data.Data (Typeable)
import Data.Generics.Product (HasType (typed))
import Data.Generics.Wrapped (wrappedTo)
import Data.Hashable (Hashable)
import qualified Data.IP
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Word (Word16, Word32, Word64)
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
  ( Action (Escape),
    ToField (toField),
  )
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
import Database.PostgreSQL.Simple.Types
  ( Default (Default),
  )
import Foreign.C (CUInt (CUInt))
import GHC.Generics (Generic)
import Inferno.Instances.Arbitrary ()
import Inferno.Types.Syntax (Ident)
import Inferno.Types.VersionControl
  ( VCObjectHash (VCObjectHash),
    vcObjectHashToByteString,
  )
import Inferno.VersionControl.Types
  ( VCMeta,
    VCObject,
    VCObjectVisibility,
  )
import Lens.Micro.Platform hiding ((.=))
import Servant
  ( Capture,
    Get,
    JSON,
    NewlineFraming,
    Put,
    QueryParam,
    QueryParam',
    ReqBody,
    Required,
    StreamPost,
    (:<|>),
    (:>),
  )
import Servant.Conduit ()
import System.Posix (EpochTime)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    choose,
    chooseInt,
    listOf,
    oneof,
    suchThat,
    vectorOf,
  )
import Test.QuickCheck.Arbitrary.ADT
  ( ADTArbitrary (ADTArbitrary),
    ADTArbitrarySingleton (ADTArbitrarySingleton),
    ConstructorArbitraryPair (ConstructorArbitraryPair),
    ToADTArbitrary
      ( toADTArbitrary,
        toADTArbitrarySingleton
      ),
    genericArbitrary,
  )
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Instances.Vector ()
import Text.Read (readMaybe)
import Web.HttpApiData
  ( FromHttpApiData (parseUrlPiece),
    ToHttpApiData (toUrlPiece),
  )

-- API type for `inferno-ml-server`
type InfernoMlServerAPI gid p =
  StatusAPI
    -- Evaluate an inference script
    :<|> InferenceAPI gid p
    :<|> InferenceTestAPI gid p
    :<|> CancelAPI

type StatusAPI =
  -- Check if the server is up and if any job is currently running
  "status" :> Get '[JSON] ServerStatus

type CancelAPI = "inference" :> "cancel" :> Put '[JSON] ()

type InferenceAPI gid p =
  "inference"
    :> "run"
    :> Capture "id" (Id (InferenceParam gid p))
    :> QueryParam "res" Int64
    :> QueryParam' '[Required] "uuid" UUID
    :> StreamPost NewlineFraming JSON (WriteStream IO)

type InferenceTestAPI gid p =
  -- Evaluate an inference script
  "inference"
    :> "test"
    :> Capture "id" (Id (InferenceParam gid p))
    :> QueryParam "res" Int64
    :> QueryParam' '[Required] "uuid" UUID
    :> ReqBody '[JSON] (EvaluationEnv gid p)
    :> StreamPost NewlineFraming JSON (WriteStream IO)

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
type WriteStream m = ConduitT () (Int, [(IValue, EpochTime)]) m ()

-- | Convenience synonym for the consumed 'WriteStream'
type Writes p = Map p [(IValue, EpochTime)]

-- | Just a convenience synonym for cleaning up type signatures
type Inputs p = Map Ident (SingleOrMany p)

-- | Same as 'Inputs' above
type Outputs p = Map Ident (SingleOrMany p)

-- | Same as 'Inputs', 'Outputs'
type Models a = Map Ident a

data ServerStatus
  = Idle
  | EvaluatingScript
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToADTArbitrary, NFData)

instance Arbitrary ServerStatus where
  arbitrary = genericArbitrary

-- | Information for contacting a bridge server that implements the 'BridgeAPI'
data BridgeInfo gid p = BridgeInfo
  { id :: Id (InferenceParam gid p)
  , host :: IPv4
  , port :: Word64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

instance FromRow (BridgeInfo gid p) where
  fromRow =
    BridgeInfo
      <$> field
      <*> field
      <*> fmap (fromIntegral @Int64) field

instance ToRow (BridgeInfo gid p) where
  toRow bi =
    [ bi.id & toField
    , bi.host & toField
    , bi.port & toField
    ]

-- | The ID of a database entity
newtype Id a = Id UUID
  deriving stock (Show, Generic)
  deriving newtype
    ( Eq
    , Ord
    , Hashable
    , FromField
    , ToField
    , FromJSON
    , ToJSON
    , FromJSONKey
    , ToJSONKey
    , ToHttpApiData
    , FromHttpApiData
    , Arbitrary
    )
  deriving anyclass (NFData, ToADTArbitrary)

-- | Row for the table containing inference script closures
data InferenceScript uid gid = InferenceScript
  { hash :: VCObjectHash
  -- ^ This is the ID for each row, stored as a @bytea@ (bytes of the hash)
  , obj :: VCMeta uid gid VCObject
  -- ^ Script closure
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToADTArbitrary)

-- Newtype just for `FromRow`/`ToRow` instances. It would be possible to just
-- add the instances to `inferno-types`, but then there would be a dependency
-- on `postgresql-simple`
newtype VCObjectHashField = VCObjectHashField VCObjectHash
  deriving stock (Generic)

instance FromField VCObjectHashField where
  fromField f = \case
    Nothing -> returnError UnexpectedNull f "Expected non-empty text column"
    Just bs ->
      fromField @ByteString f (Just bs) >>= \case
        b
          | Right base64 <- Base64.URL.decode b
          , Just digest <- digestFromByteString base64 ->
              pure . VCObjectHashField $ VCObjectHash digest
        _ -> returnError ConversionFailed f "Invalid VC object hash"

instance ToField VCObjectHashField where
  toField = Escape . vcObjectHashToByteString . wrappedTo

instance (ToJSON uid, ToJSON gid) => ToRow (InferenceScript uid gid) where
  toRow s =
    -- NOTE: Don't change the order!
    [ s.hash & VCObjectHashField & toField
    , s.obj & Aeson & toField
    ]

instance
  ( FromJSON uid
  , FromJSON gid
  , Typeable uid
  , Typeable gid
  ) =>
  FromRow (InferenceScript uid gid)
  where
  fromRow =
    InferenceScript
      <$> fmap wrappedTo (field @VCObjectHashField)
      <*> fmap getAeson field

instance
  ( Arbitrary uid
  , Arbitrary gid
  ) =>
  Arbitrary (InferenceScript uid gid)
  where
  arbitrary = genericArbitrary

-- | Row of the model table, parameterized by the user and group type. This
-- table contains metadata for models that should not change between different
-- versions, e.g. model name and permissions. A second table, 'ModelVersion',
-- contains the specific versions of each model (and the actual model contents),
-- along with other metadata that may change between versions
data Model gid = Model
  { id :: Maybe (Id (Model gid))
  , name :: Text
  , gid :: gid
  -- ^ The group that owns this model
  , visibility :: VCObjectVisibility
  -- ^ Analogous to visibility of @inferno-vc@ scripts
  , created :: Maybe UTCTime
  -- ^ When the model was created; if left empty, this will be generated
  -- automatically when the model is saved
  , updated :: Maybe UTCTime
  -- ^ The last time the model was updated (i.e. a new version was created),
  -- if any
  , terminated :: Maybe UTCTime
  -- ^ The time that this model was \"deleted\", if any. For active models,
  -- this will be @Nothing@
  }
  deriving stock (Show, Eq, Generic)

instance NFData (Model gid) where
  rnf = rwhnf

instance
  ( Typeable gid
  , FromField gid
  , Ord gid
  ) =>
  FromRow (Model gid)
  where
  -- NOTE: Order of fields must align exactly with DB schema
  fromRow =
    Model
      <$> field
      <*> field
      <*> field
      <*> fmap getAeson field
      <*> field
      <*> field
      <*> field

instance (ToField gid) => ToRow (Model gid) where
  -- NOTE: Order of fields must align exactly with DB schema
  toRow m =
    [ -- Normally the ID will be missing for new rows, in that case the default
      -- will be used (a random UUID). But in other cases it is useful to set
      -- the ID explicitly (e.g. for testing)
      m.id & maybe (toField Default) toField
    , m.name & toField
    , m.gid & toField
    , m.visibility & Aeson & toField
    , m.created & maybe (toField Default) toField
    , -- The `ToRow` instance is only for new rows, so we don't want to set
      -- the `updated` and `terminated` fields to anything by default
      --
      -- The same rationale applies to the other `toField Default`s for
      -- different types below
      toField Default
    , toField Default
    ]

instance
  ( FromJSON gid
  , Ord gid
  ) =>
  FromJSON (Model gid)
  where
  parseJSON = withObject "Model" $ \o ->
    Model
      -- If a new model is being created, its ID will not be present
      <$> o .:? "id"
      <*> o .: "name"
      <*> o .: "gid"
      <*> o .: "visibility"
      <*> o .:? "created"
      <*> o .:? "updated"
      -- If a new model is being serialized, it does not really make
      -- sense to require a `"terminated": null` field
      <*> o .:? "terminated"

instance (ToJSON gid) => ToJSON (Model gid) where
  toJSON m =
    object
      [ "id" .= m.id
      , "name" .= m.name
      , "gid" .= m.gid
      , "visibility" .= m.visibility
      , "created" .= m.created
      , "updated" .= m.updated
      , "terminated" .= m.terminated
      ]

-- Not derived generically in order to use special `Gen UTCTime`
instance (Ord gid, Arbitrary gid) => Arbitrary (Model gid) where
  arbitrary =
    Model
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genMUtc
      <*> genMUtc
      <*> genMUtc

-- Can't be derived because there is (intentially) no `Arbitrary UTCTime` in scope
instance (Arbitrary gid, Ord gid) => ToADTArbitrary (Model gid) where
  toADTArbitrarySingleton _ =
    ADTArbitrarySingleton "Inferno.ML.Server.Types" "Model"
      . ConstructorArbitraryPair "Model"
      <$> arbitrary

  toADTArbitrary _ =
    ADTArbitrary "Inferno.ML.Server.Types" "Model"
      <$> sequence [ConstructorArbitraryPair "Model" <$> arbitrary]

-- | Represents rows of the model version tables; each row is linked to its
-- 'Model' parent and also contains the actual contents of the model. This
-- is parameterized by the user and group types as well as the type of the
-- content, which will normally be an 'Oid' (Postgres large object). Other
-- model metadata is contained here as well, e.g. the model card, as this
-- might change between versions
data ModelVersion gid c = ModelVersion
  { id :: Maybe (Id (ModelVersion gid c))
  , model :: Id (Model gid)
  -- ^ Foreign key of the @model@ table, which contains invariant metadata
  -- related to the model, i.e. name, permissions, user
  , description :: Text
  , card :: ModelCard
  , contents :: c
  -- ^ The actual contents of version of the model. Normally this will be
  -- an 'Oid' pointing to the serialized bytes of the model imported into
  -- the PSQL large object table
  , size :: Word64
  -- ^ The size of the @contents@ above; the contents are immutable so we can
  -- calculate this once and store it
  , version :: Version
  , created :: Maybe UTCTime
  -- ^ When the model version was created; if left empty, this will be generated
  -- automatically when the model version is saved
  , terminated :: Maybe UTCTime
  -- ^ The time that this model version was \"deleted\", if any. For active
  -- models versions, this will be @Nothing@
  }
  deriving stock (Show, Eq, Generic)
  -- NOTE: This may require an orphan instance for the `c` type variable
  deriving anyclass (NFData)

instance (FromField gid) => FromRow (ModelVersion gid Oid) where
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
      -- The actual storage type is a `bigint` because a regular `integer` may
      -- not be big enough (only large enough for ~2gb size). We can't have a
      -- negative model size though, so this is represented as a `Word64` on
      -- the Haskell side
      <*> fmap fromIntegral (field @Int64)
      <*> field
      <*> field
      <*> field

instance (ToField gid) => ToRow (ModelVersion gid Oid) where
  -- NOTE: Order of fields must align exactly with DB schema
  toRow mv =
    [ mv.id & maybe (toField Default) toField
    , mv.model & toField
    , mv.description & toField
    , mv.card & Aeson & toField
    , mv.contents & toField
    , mv.size & toField
    , mv.version & toField
    , mv.created & maybe (toField Default) toField
    , toField Default
    ]

instance (FromJSON gid) => FromJSON (ModelVersion gid Oid) where
  parseJSON = withObject "ModelVersion" $ \o ->
    ModelVersion
      <$> o .:? "id"
      <*> o .: "model"
      <*> o .: "description"
      <*> o .: "card"
      <*> fmap (Oid . fromIntegral @Word64) (o .: "contents")
      <*> o .: "size"
      <*> o .: "version"
      -- Will be absent when saving new model version
      <*> o .:? "created"
      -- If a new model version is being serialized, it does not really make
      -- sense to require a `"terminated": null` field
      <*> o .:? "terminated"

instance (ToJSON gid) => ToJSON (ModelVersion gid Oid) where
  toJSON mv =
    object
      [ "id" .= mv.id
      , "model" .= mv.model
      , "description" .= mv.description
      , "contents" .= unOid mv.contents
      , "size" .= mv.size
      , "version" .= mv.version
      , "card" .= mv.card
      , "created" .= mv.created
      , "terminated" .= mv.terminated
      ]
    where
      unOid :: Oid -> Word32
      unOid (Oid (CUInt x)) = x

-- Not derived generically in order to use special `Gen UTCTime`
instance (Arbitrary c) => Arbitrary (ModelVersion gid c) where
  arbitrary =
    ModelVersion
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genMUtc
      <*> genMUtc

-- Can't be derived because there is (intentially) no `Arbitrary UTCTime` in scope
instance (Arbitrary c) => ToADTArbitrary (ModelVersion gid c) where
  toADTArbitrarySingleton _ =
    ADTArbitrarySingleton "Inferno.ML.Server.Types" "ModelVersion"
      . ConstructorArbitraryPair "ModelVersion"
      <$> arbitrary

  toADTArbitrary _ =
    ADTArbitrary "Inferno.ML.Server.Types" "ModelVersion"
      <$> sequence [ConstructorArbitraryPair "ModelVersion" <$> arbitrary]

-- | Full description and metadata of the model
data ModelCard = ModelCard
  { summary :: ModelSummary
  -- ^ High-level, structured overview of model details and summary
  , metadata :: ModelMetadata
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData, ToADTArbitrary)
  deriving (FromField, ToField) via Aeson ModelCard

instance Arbitrary ModelCard where
  arbitrary = genericArbitrary

-- | Structured summary of a model
data ModelSummary = ModelSummary
  { summary :: Text
  -- ^ General summary of model (longer than top-level @description@ field
  -- of 'ModelVersion' type)
  , uses :: Text
  -- ^ How the model is intended to be used
  , evaluation :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, NFData, ToADTArbitrary)

instance FromJSON ModelSummary where
  parseJSON = withObject "ModelSummary" $ \o ->
    ModelSummary
      <$> o .: "summary"
      <*> o .:? "uses" .!= mempty
      <*> o .:? "evaluation" .!= mempty

instance Arbitrary ModelSummary where
  arbitrary = genericArbitrary

-- | Metadata for the model, inspired by Hugging Face model card format
data ModelMetadata = ModelMetadata
  { categories :: Vector Int
  , datasets :: Text
  , metrics :: Text
  , baseModel :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance NFData ModelMetadata where
  rnf = rwhnf

instance FromJSON ModelMetadata where
  parseJSON = withObject "ModelMetadata" $ \o ->
    ModelMetadata
      <$> o .:? "categories" .!= mempty
      <*> o .:? "datasets" .!= mempty
      <*> o .:? "metrics" .!= mempty
      <*> o .:? "base-model"

instance ToJSON ModelMetadata where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '-'
        , omitNothingFields = True
        }

instance Arbitrary ModelMetadata where
  arbitrary =
    ModelMetadata
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance ToADTArbitrary ModelMetadata where
  toADTArbitrarySingleton _ =
    ADTArbitrarySingleton "Inferno.ML.Server.Types" "ModelMetadata"
      . ConstructorArbitraryPair "ModelMetadata"
      <$> arbitrary

  toADTArbitrary _ =
    ADTArbitrary "Inferno.ML.Server.Types" "ModelMetadata"
      <$> sequence [ConstructorArbitraryPair "ModelMetadata" <$> arbitrary]

-- | Similar to the @Version@ type from base, but allows for a leading @v@ and
-- guarantees that there is at least one digit. Digits must be separated by @.@;
-- multiple tags are allowed, separated by @-@
data Version
  = Version
      -- | List of digits for version string
      (NonEmpty Int)
      -- | Any tags
      [Text]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, ToADTArbitrary)

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

instance Arbitrary Version where
  arbitrary =
    Version
      <$> genDigits
      -- This can't be some arbitrary text, otherwise JSON parsing will fail
      <*> genTags
    where
      genDigits :: Gen (NonEmpty Int)
      genDigits = fmap (fmap abs) $ arbitrary `suchThat` ((<= 5) . length)

      genTags :: Gen [Text]
      genTags = listOf asciiTextGen `suchThat` ((<= 5) . length)
        where
          asciiTextGen :: Gen Text
          asciiTextGen = fmap Text.pack . vectorOf 5 $ chr <$> choose (97, 122)

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
          (Attoparsec.takeWhile (Attoparsec.inClass "a-zA-Z"))
        $ Attoparsec.char '-'

showVersion :: Version -> Text
showVersion (Version ns ts) =
  mconcat
    [ "v"
    , Text.intercalate "." . fmap tshow $ NonEmpty.toList ns
    , bool ("-" <> Text.intercalate "-" ts) mempty $ null ts
    ]

-- | Row of the inference parameter table, parameterized by the user, group, and
-- script type
data InferenceParam gid p = InferenceParam
  { id :: Maybe (Id (InferenceParam gid p))
  , script :: VCObjectHash
  -- ^ The script of the parameter
  , inputs :: Inputs p
  -- ^ Mapping the input\/output to the Inferno identifier helps ensure that
  -- Inferno identifiers are always pointing to the correct input\/output;
  -- otherwise we would need to rely on the order of the original identifiers
  , outputs :: Outputs p
  , resolution :: Word64
  -- ^ Resolution, passed to bridge routes
  , terminated :: Maybe UTCTime
  -- ^ The time that this parameter was \"deleted\", if any. For active
  -- parameters, this will be @Nothing@
  , gid :: gid
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, ToJSON)

instance (FromJSON p, FromJSON gid) => FromJSON (InferenceParam gid p) where
  parseJSON = withObject "InferenceParam" $ \o ->
    InferenceParam
      -- The ID needs to be included when deserializing
      <$> o .: "id"
      <*> o .: "script"
      <*> o .: "inputs"
      <*> o .: "outputs"
      <*> o .:? "resolution" .!= 128
      -- We shouldn't require this field
      <*> o .:? "terminated"
      <*> o .: "gid"

instance
  ( FromJSON p
  , FromField gid
  , Typeable gid
  , Typeable p
  ) =>
  FromRow (InferenceParam gid p)
  where
  fromRow =
    InferenceParam
      <$> field
      <*> fmap wrappedTo (field @VCObjectHashField)
      <*> fmap getAeson field
      <*> fmap getAeson field
      <*> fmap fromIntegral (field @Int64)
      <*> field
      <*> field

instance (ToJSON p, ToField gid) => ToRow (InferenceParam gid p) where
  -- NOTE: Do not change the order of the field actions
  toRow ip =
    [ ip.id & maybe (toField Default) toField
    , ip.script & VCObjectHashField & toField
    , ip.inputs & Aeson & toField
    , ip.outputs & Aeson & toField
    , ip.resolution & Aeson & toField
    , toField Default
    , ip.gid & toField
    ]

-- Not derived generically in order to use special `Gen UTCTime`
instance (Arbitrary gid, Arbitrary p) => Arbitrary (InferenceParam gid p) where
  arbitrary =
    InferenceParam
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genMUtc
      <*> arbitrary

-- Can't be derived because there is (intentially) no `Arbitrary UTCTime` in scope
instance (Arbitrary gid, Arbitrary p) => ToADTArbitrary (InferenceParam gid p) where
  toADTArbitrarySingleton _ =
    ADTArbitrarySingleton "Inferno.ML.Server.Types" "InferenceParam"
      . ConstructorArbitraryPair "InferenceParam"
      <$> arbitrary

  toADTArbitrary _ =
    ADTArbitrary "Inferno.ML.Server.Types" "InferenceParam"
      <$> sequence [ConstructorArbitraryPair "InferenceParam" <$> arbitrary]

-- | An 'InferenceParam' together with all of the model versions that are
-- linked to it indirectly via its script. This is provided for convenience
data InferenceParamWithModels gid p = InferenceParamWithModels
  { param :: InferenceParam gid p
  , models :: Models (Id (ModelVersion gid Oid))
  }
  deriving stock (Show, Eq, Generic)

-- | Information about execution time and resource usage. This is saved by
-- @inferno-ml-server@ after script evaluation completes and can be queried
-- later by using the same job identifier that was provided to the @/inference@
-- route
data EvaluationInfo gid p = EvaluationInfo
  { id :: UUID
  -- ^ Note that this is the job identifier provided to the inference
  -- evaluation route, and is also the primary key of the database table
  , param :: Id (InferenceParam gid p)
  , start :: UTCTime
  -- ^ When inference evaluation started
  , end :: UTCTime
  -- ^ When inference evaluation ended
  , allocated :: Word64
  -- ^ The number of bytes allocated between the @start@ and @end@. Note
  -- that this is /total/ allocation over the course of evaluation, which
  -- can be many times greater than peak memory usage. Nevertheless, this
  -- can be useful to track memory usage over time and across different
  -- script revisions
  , cpu :: Word64
  -- ^ Additional CPU time used between the @start@ and @end@. This is
  -- converted from picoseconds to milliseconds
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance FromRow (EvaluationInfo gid p) where
  fromRow =
    EvaluationInfo
      <$> field
      <*> field
      <*> field
      <*> field
      <*> fmap (fromIntegral @Int64) field
      <*> fmap (fromIntegral @Int64) field

instance ToRow (EvaluationInfo gid p) where
  toRow ei =
    [ ei.id & toField
    , ei.param & toField
    , ei.start & toField
    , ei.end & toField
    , ei.allocated & toField
    , ei.cpu & toField
    ]

-- Not derived generically in order to use special `Gen UTCTime`
instance Arbitrary (EvaluationInfo gid p) where
  arbitrary =
    EvaluationInfo
      <$> arbitrary
      <*> arbitrary
      <*> genUtc
      <*> genUtc
      <*> arbitrary
      <*> arbitrary

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

instance Arbitrary IPv4 where
  arbitrary = genFromOctects

instance ToADTArbitrary IPv4 where
  toADTArbitrarySingleton _ =
    ADTArbitrarySingleton "Inferno.ML.Server.Types" "IPv4"
      . ConstructorArbitraryPair "IPv4"
      <$> arbitrary

  toADTArbitrary _ =
    ADTArbitrary "Inferno.ML.Server.Types" "IPv4"
      <$> sequence [ConstructorArbitraryPair "IPv4" <$> arbitrary]

genFromOctects :: Gen IPv4
genFromOctects =
  toIPv4
    <$> ((,,,) <$> octetGen <*> octetGen <*> octetGen <*> octetGen)

octetGen :: Gen Int
octetGen = choose (0, 255)

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
  | IInt Int64
  | IWord16 Word16
  | IWord32 Word32
  | IWord64 Word64
  | IDouble Double
  | IBool Bool
  | ITime EpochTime
  | IArray (Vector IValue)
  | ITuple (IValue, IValue)
  | IEmpty
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance FromJSON IValue where
  parseJSON =
    genericParseJSON
      defaultOptions
        { -- `t` for type and `v` for value; it may seem a bit cryptic but
          -- this is not for human consumption and using the abbreviations
          -- reduces space for each item in the stream
          sumEncoding = TaggedObject "t" "v"
        }

instance ToJSON IValue where
  toJSON =
    genericToJSON
      defaultOptions
        { -- See note above
          sumEncoding = TaggedObject "t" "v"
        }

-- | Used to represent inputs to the script. 'Many' allows for an array input
data SingleOrMany a
  = Single a
  | Many (Vector a)
  deriving stock (Show, Eq, Generic, Functor)
  deriving anyclass (NFData, ToADTArbitrary)

instance (Arbitrary a) => Arbitrary (SingleOrMany a) where
  arbitrary = genericArbitrary

instance (FromJSON a) => FromJSON (SingleOrMany a) where
  parseJSON v =
    asum
      [ Single <$> parseJSON v
      , Many <$> parseJSON v
      ]

instance (ToJSON a) => ToJSON (SingleOrMany a) where
  toJSON = \case
    Single a -> toJSON a
    Many as -> toJSON as

instance (Ord a) => Ord (SingleOrMany a) where
  compare a =
    (a,) >>> \case
      (Single x, Single y) -> compare x y
      (Many xs, Many ys) -> compare xs ys
      (Single _, Many _) -> LT
      (Many _, Single _) -> GT

-- | An environment that can be used to override the @inferno-ml-server@ script
-- evaluator. This allows for more interactive testing
data EvaluationEnv gid p = EvaluationEnv
  { script :: VCObjectHash
  , inputs :: Inputs p
  , outputs :: Outputs p
  , models :: Models (Id (ModelVersion gid Oid))
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToADTArbitrary)

instance (Arbitrary p) => Arbitrary (EvaluationEnv gid p) where
  arbitrary = genericArbitrary

data RemoteError p m mv
  = CacheSizeExceeded
  | -- | Either parent model row corresponding to the model version, or the
    -- the requested model version itself, does not exist
    NoSuchModel (Either (Id m) (Id mv))
  | NoSuchScript VCObjectHash
  | NoSuchParameter (Id p)
  | InvalidScript (Id p) Text
  | InvalidOutput (Id p) Text
  | -- | Any error condition returned by Inferno script evaluation
    InfernoError (Id p) VCObjectHash SomeInfernoError
  | NoBridgeSaved (Id p)
  | ScriptTimeout (Id p) Int
  | -- | A script evaluation has used too much memory and has been killed
    -- (in-process), pre-empting systemd OOM
    MemoryLimitExceeded
      -- | Actual memory usage at time of exception
      Word64
  | DbError String
  | ClientError (Id p) String
  | OtherRemoteError Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype SomeInfernoError = SomeInfernoError String
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Exception, ToJSON, FromJSON)

instance (Typeable p, Typeable m, Typeable mv) => Exception (RemoteError p m mv) where
  displayException = \case
    CacheSizeExceeded -> "Model exceeds maximum cache size"
    NoSuchModel (Left m) ->
      unwords
        [ "Model:"
        , "'" <> show m <> "'"
        , "does not exist in the store"
        ]
    NoSuchModel (Right mv) ->
      unwords
        [ "Model version:"
        , "'" <> show mv <> "'"
        , "does not exist in the store"
        ]
    NoSuchScript vch ->
      unwords
        [ "Script identified by hash"
        , show vch
        , "does not exist"
        ]
    NoSuchParameter iid ->
      unwords ["Parameter:", "'" <> show iid <> "'", "does not exist"]
    InvalidScript ipid t ->
      unwords
        [ "Invalid script when evaluating"
        , show ipid
        , ":"
        , Text.unpack t
        ]
    InvalidOutput ipid t ->
      unwords
        [ "When evaluating"
        , show ipid
        , "script output should be an array of `write` but was"
        , Text.unpack t
        ]
    InfernoError ipid vch (SomeInfernoError x) ->
      unwords
        [ "Parameter"
        , show ipid
        , "failed to evaluate script"
        , show vch
        , "with:"
        , x
        ]
    NoBridgeSaved ipid ->
      unwords
        [ "No bridge for "
        , show ipid
        , "has been saved"
        ]
    ScriptTimeout ipid t ->
      unwords
        [ "Script evaluation for"
        , show ipid
        , "timed out after"
        , show $ t `div` 1000000
        , "seconds"
        ]
    MemoryLimitExceeded usage ->
      unwords
        [ "Memory usage of script evaluator has exceeded limit,"
        , "script evaluation memory usage:"
        , -- This is to make the error more user-friendly. Instead of seeing
          -- the actual bytes as a `Word64`, this will produce a nicer
          -- `"... GB"` string (potentially fractional)
          ByteUnits.getShortHand $
            ByteUnits.convertByteUnit
              (ByteUnits.ByteValue (realToFrac usage) ByteUnits.Bytes)
              ByteUnits.GigaBytes
        ]
    DbError e ->
      unwords
        [ "Database error:"
        , e
        ]
    ClientError ipid ce ->
      unwords
        [ "Client error when calculating"
        , show ipid
        , ":"
        , ce
        ]
    OtherRemoteError e -> Text.unpack e

data RemoteTrace p m mv
  = InfoTrace (TraceInfo p mv)
  | WarnTrace (TraceWarn p)
  | ErrorTrace (RemoteError p m mv)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving (FromField, ToField) via Aeson (RemoteTrace p m mv)

showTrace ::
  ( Typeable p
  , Typeable m
  , Typeable mv
  ) =>
  RemoteTrace p m mv -> Text
showTrace = \case
  InfoTrace i -> case i of
    StartingServer -> "Starting `inferno-ml-server`"
    RunningInference ipid t ->
      Text.unwords
        [ "Running inference param:"
        , tshow ipid <> ","
        , "with timeout:"
        , tshow $ t `div` 1000000
        , "(seconds)"
        ]
    EvaluatingParam s ->
      Text.unwords
        [ "Evaluating inferno script for parameter:"
        , tshow s
        ]
    CopyingModel m ->
      Text.unwords
        [ "Copying model to cache:"
        , tshow m
        ]
    OtherInfo t -> t
    -- Note this will not actually be logged by the server, but is useful for
    -- DB tracing
    ExternalTrace t -> "(external) " <> t
  WarnTrace w -> case w of
    CancelingInference i ->
      Text.unwords
        [ "Canceling inference job for param:"
        , tshow i
        ]
    CouldntMoveTensor dev ->
      Text.pack $
        unwords
          [ "Couldn't move tensor to device"
          , dev
          ]
    OomKilled time ->
      Text.pack $
        unwords
          [ "Server is restarting from out-of-memory event"
          , "triggered at"
          , show time
          ]
    CantMonitorMemory details ->
      Text.unwords
        [ details <> ","
        , "cannot monitor memory usage;"
        , "running action without memory monitoring"
        ]
    OtherWarn t -> t
  ErrorTrace e -> Text.pack $ displayException e

showTraceWithLevel ::
  ( Typeable p
  , Typeable m
  , Typeable mv
  ) =>
  RemoteTrace p m mv -> Text
showTraceWithLevel = \case
  i@InfoTrace{} -> "[INFO] " <> showTrace i
  w@WarnTrace{} -> "[WARN] " <> showTrace w
  e@ErrorTrace{} -> "[ERROR] " <> showTrace e

data TraceInfo p mv
  = StartingServer
  | RunningInference (Id p) Int
  | EvaluatingParam (Id p)
  | CopyingModel (Id mv)
  | OtherInfo Text
  | -- | This would be generated by the external orchestration system that runs
    -- the @inferno-ml-server@ instances. It is not created by the running server.
    -- It is separate from @OtherInfo@ to make clear that it comes from an external
    -- source. Using this we can save traces that precede the server start
    -- (or come after it shuts down, etc...)
    ExternalTrace Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TraceWarn p
  = CancelingInference (Id p)
  | -- | The tensor could not be moved from the device. The device is represented
    -- as a string here so that we don't need to use @Device@ from Hasktorch
    -- (which would pose build problems for us elsewhere)
    --
    -- Normally this would occur if a CPU-only @inferno-ml-server@ tried
    -- moving a tensor from \"cpu:0\" to \"cuda:0\". We don\'t throw any
    -- exceptions in that case, but the log is still helpful
    CouldntMoveTensor String
  | -- | @inferno-ml-server@ was last killed due to an OOM event; the time at
    -- restart is recorded
    OomKilled UTCTime
  | -- | Memory monitoring will not work due to e.g. missing cgroup information
    CantMonitorMemory
      -- | Details
      Text
  | OtherWarn Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

maybeConversion ::
  (Typeable b) => (a -> Maybe b) -> Field -> Maybe a -> Conversion b
maybeConversion f fld =
  maybe (returnError UnexpectedNull fld mempty) $
    maybe (returnError ConversionFailed fld mempty) pure
      . f

genMUtc :: Gen (Maybe UTCTime)
genMUtc = oneof [Just <$> genUtc, pure Nothing]

-- This provides a reasonable timestamp rounded to the second, instead of
-- having fractional seconds as when using the `Arbitrary UTCTime` instance
-- from `Test.QuickCheck.Instances.Time`
genUtc :: Gen UTCTime
genUtc =
  posixSecondsToUTCTime . realToFrac <$> chooseInt (1420000000, 1720000000)
