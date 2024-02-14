{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Inferno.ML.Server.Types where

import Conduit (ConduitT)
import Control.Applicative (asum, optional)
import Control.Category ((>>>))
import Control.DeepSeq (NFData (rnf), rwhnf)
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.Bool (bool)
import Data.Char (toLower)
import Data.Data (Typeable)
import Data.Generics.Labels ()
import Data.Generics.Product (HasType (typed))
import Data.IP (IPv4)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Scientific (Scientific, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word32, Word64)
import Database.PostgreSQL.Simple.FromField
  ( FromField (fromField),
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
import Database.PostgreSQL.Simple.Types (Default (Default))
import Foreign.C (CUInt (CUInt))
import GHC.Generics (Generic)
import Inferno.ML.Server.Types.Orphans ()
import Inferno.Types.VersionControl (VCObjectHash)
import Inferno.VersionControl.Types (VCMeta, VCObject)
import Lens.Micro.Platform hiding ((.=))
import Servant
  ( Capture,
    Get,
    JSON,
    NewlineFraming,
    Post,
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
import URI.ByteString (Absolute, URIRef)
import URI.ByteString.Aeson ()
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

type InfernoMlServerAPI uid gid p s =
  -- Check if the server is up and if any job is currently running:
  --
  --  * `Nothing` -> The server is evaluating a script
  --  * `Just ()` -> The server is not doing anything and can be killed
  --
  -- This can be implemented using an `MVar ()`
  "status" :> Get '[JSON] (Maybe ())
    -- Evaluate an inference script. The script must evaluate to a tensor, which
    -- will then be converted to an array, which will subsequently be streamed in chunks
    --
    -- NOTE: The endpoint streams back individual chunks of the list with the same
    -- number of dimensions as the converted tensor. The resulting lists can then be
    -- concatenated to recover the original list
    --
    -- For example, the following output tensor
    --
    -- `[ [ 0.0, 0.0, ...], [ 0.0, 0.0, ...], ...]`
    --
    -- will be converted to
    --
    -- `Twos @Float [ [ 0.0, 0.0, ...], [ 0.0, 0.0, ...], ...]`
    --
    -- and then streamed back in the conduit as individual chunks of `Twos`, e.g.
    --
    -- `Twos @Float [ [ 0.0, 0.0, ...] ]`
    --
    -- `sinkList` or similar can be used to collect all of the individual chunks,
    --  which, when concatenated, will evaluate to the converted tensor
    --
    -- NOTE: Each output tensor is converted to a list of `Scientific`. This is
    -- to avoid dealing with any variables in the API type, or existentials, etc...
    -- Since the output will be serialized as JSON anyway, where conversion to
    -- `Scientific`s will already take place, it is more convenient to explicitly
    -- return this
    :<|> "inference"
      :> Capture "id" (Id (InferenceParam uid gid p s))
      :> QueryParam "res" Int64
      :> StreamPost NewlineFraming JSON (TStream Scientific IO)
    :<|> "inference" :> "cancel" :> Put '[JSON] ()
    -- Register the bridge. This is an `inferno-ml-server` endpoint, not a
    -- bridge endpoint
    :<|> "bridge" :> ReqBody '[JSON] BridgeInfo :> Post '[JSON] ()
    -- Check for bridge registration
    :<|> "bridge" :> Get '[JSON] (Maybe BridgeInfo)

-- Stream of tensor elements
type TStream a m = ConduitT () (AsValue a) m ()

-- A bridge to get data for use with Inferno scripts. This is implemented by
-- the bridge, not by `inferno-ml-server`
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

data BridgeInfo = BridgeInfo
  { host :: IPv4,
    port :: Word64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Convenience type for dealing with 'AsValue's, rather than pattern matching
-- on the @dtype@ inside the 'AsValue', as well as allowing different numerical
-- representations inside the same type
data AsValueTyped
  = Floats (AsValue Float)
  | Doubles (AsValue Double)
  | Int64s (AsValue Int64)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

toAsValueTyped :: AsValue Scientific -> AsValueTyped
toAsValueTyped (AsValue dt xs) = case dt of
  Float -> Floats . AsValue dt $ toRealFloat <$> xs
  Double -> Doubles . AsValue dt $ toRealFloat <$> xs
  -- NOTE: As long as the `AsValue` is only obtained from the streaming endpoint
  -- above, using `round` should be fine (since the `Scientific` was originally
  -- an `Int64` anyway). Using `round` avoids the `Maybe` context of other ways of
  -- converting from a `Scientific` to an integral type
  Int64 -> Int64s . AsValue dt $ round <$> xs

instance ToJSON AsValueTyped where
  toJSON = \case
    Floats xs -> mkObj Float xs
    Doubles xs -> mkObj Double xs
    Int64s xs -> mkObj Int64 xs
    where
      mkObj :: ToJSON a => DType -> AsValue a -> Value
      mkObj t xs =
        object
          [ "dtype" .= t,
            "value" .= xs
          ]

instance FromJSON AsValueTyped where
  parseJSON = withObject "AsValueTyped" $ \o ->
    o .: "dtype" >>= \case
      Float -> Floats <$> o .: "value"
      Double -> Doubles <$> o .: "value"
      Int64 -> Int64s <$> o .: "value"

-- | A converted output tensor, tagged with its 'DType' and holding a list ('Dims')
-- representing the original tensor. Using this representation keeps track of both
-- the number of dimensions and the datatype, which must be retained when serializing
-- the list
data AsValue a = AsValue
  { dtype :: DType,
    values :: Dims a
  }
  deriving stock (Show, Eq, Generic, Functor)
  deriving anyclass (FromJSON, NFData)

instance ToJSON a => ToJSON (AsValue a) where
  -- NOTE: See the note on the `ToJSON` instance for `Dims` below
  toEncoding (AsValue dt xs) =
    pairs $
      mconcat
        [ "dtype" .= dt,
          "values" .= xs
        ]

catAsValues :: AsValue a -> AsValue a -> Maybe (AsValue a)
catAsValues (AsValue dt1 x) (AsValue dt2 y)
  | dt1 == dt2 = case (x, y) of
      (Ones xs, Ones ys) -> Just . AsValue dt1 . Ones $ xs <> ys
      (Twos xs, Twos ys) -> Just . AsValue dt1 . Twos $ xs <> ys
      (Threes xs, Threes ys) -> Just . AsValue dt1 . Threes $ xs <> ys
      (Fours xs, Fours ys) -> Just . AsValue dt1 . Fours $ xs <> ys
      _ -> Nothing
  | otherwise = Nothing

-- | The actual converted output tensor (i.e. from @Torch.asValue@), up to a fixed
-- number of dimensions
data Dims a
  = Ones [a]
  | Twos [[a]]
  | Threes [[[a]]]
  | Fours [[[[a]]]]
  deriving stock (Show, Eq, Generic, Functor)
  deriving anyclass (NFData)

instance FromJSON a => FromJSON (Dims a) where
  parseJSON = withArray "Dims" $ \a ->
    asum
      [ Ones <$> parseJSON (Array a),
        Twos <$> parseJSON (Array a),
        Threes <$> parseJSON (Array a),
        Fours <$> parseJSON (Array a)
      ]

instance ToJSON a => ToJSON (Dims a) where
  -- NOTE
  -- `toEncoding` must be used to retain the correct serialized representation of
  -- numeric values. For example, `toJSON @Float 0.0 = "0"`, but
  -- `toEncoding @Float 0.0 = "0.0"`. There may be decoding issues later
  -- if this is not done
  toEncoding = \case
    Ones xs -> toEncoding xs
    Twos xs -> toEncoding xs
    Threes xs -> toEncoding xs
    Fours xs -> toEncoding xs

  toJSON = \case
    Ones xs -> toJSON xs
    Twos xs -> toJSON xs
    Threes xs -> toJSON xs
    Fours xs -> toJSON xs

-- | Supported tensor datatypes.
data DType
  = Int64
  | Float
  | Double
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance FromJSON DType where
  parseJSON = withText "DType" $ \case
    "int" -> pure Int64
    "float" -> pure Float
    "double" -> pure Double
    d -> fail $ unwords ["Invalid dtype:", show d]

instance ToJSON DType where
  toJSON =
    String . \case
      Int64 -> "int"
      dt -> Text.pack $ toLower <$> show dt

-- | Tensor dimension
data Dim
  = One
  | Two
  | Three
  | Four
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance FromJSON Dim where
  parseJSON = withScientific "Dim" $ \case
    1 -> pure One
    2 -> pure Two
    3 -> pure Three
    4 -> pure Four
    n -> fail $ unwords ["Dim out of range:", show n]

instance ToJSON Dim where
  toJSON =
    Number . \case
      One -> 1
      Two -> 2
      Three -> 3
      Four -> 4

-- | The ID of a database entity
newtype Id a = Id Int64
  deriving stock (Show, Generic)
  deriving newtype
    ( Eq,
      FromField,
      ToField,
      FromJSON,
      ToJSON,
      ToHttpApiData,
      FromHttpApiData
    )
  deriving anyclass (NFData)

-- | Row for the table containing inference script closures
data InferenceScript uid gid = InferenceScript
  { -- NOTE: This is the ID for each row
    hash :: VCObjectHash,
    obj :: VCMeta uid gid VCObject
  }
  deriving stock (Show, Eq, Generic)

-- The `ToRow` instance can recycle the `ToJSON` instances (for both field)
instance (ToJSON uid, ToJSON gid) => ToRow (InferenceScript uid gid) where
  toRow s =
    -- NOTE: Don't change the order!
    [ s ^. #hash & toField,
      s ^. #obj & Aeson & toField
    ]

-- The `FromRow` instance can also recycle the Aeson instances
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
      <$> field
      <*> fmap getAeson field

-- Row of the model table, parameterized by the user and group type as well
-- as the contents of the model (normally this will be an `Oid`)
data Model uid gid c = Model
  { id :: Maybe (Id (Model uid gid c)),
    name :: Text,
    -- | The actual contents of the model
    contents :: c,
    version :: Version,
    -- | Stored as JSON in the DB
    card :: ModelCard,
    -- | Permissions for reading or updating the model, organized by group ID
    --
    -- NOTE
    -- This is stored as a @jsonb@ rather than as @hstore@. It could
    -- currently be stored as an @hstore@, but later we might want to
    -- use a more complex type that we could not easily convert to\/from
    -- text (which is required to use @hstore@). So using @jsonb@ allows
    -- for greater potential flexibility
    permissions :: Map gid ModelPermissions,
    -- | The user who owns the model, if any. Note that owning a model
    -- will implicitly set permissions
    user :: Maybe uid
  }
  deriving stock (Show, Eq, Generic)

instance NFData (Model uid gid c) where
  rnf = rwhnf

instance
  ( Typeable uid,
    Typeable gid,
    FromField uid,
    FromField gid,
    FromJSONKey gid,
    Ord gid
  ) =>
  FromRow (Model uid gid Oid)
  where
  -- NOTE
  -- Order of fields must align exactly with DB schema
  fromRow =
    Model
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> fmap getAeson field
      <*> field

instance
  ( Typeable uid,
    Typeable gid,
    ToField uid,
    ToField gid,
    ToJSONKey gid
  ) =>
  ToRow (Model uid gid Oid)
  where
  -- NOTE
  -- Order of fields must align exactly with DB schema
  toRow m =
    [ toField Default,
      m ^. #name & toField,
      m ^. #contents & toField,
      m ^. #version & toField,
      m ^. #card & toField,
      m ^. #permissions & Aeson & toField,
      m ^. #user & toField
    ]

{- ORMOLU_DISABLE -}
instance
  ( FromJSON uid,
    FromJSON gid,
    FromJSONKey gid,
    Ord gid
  ) =>
  FromJSON (Model uid gid Oid)
  where
  parseJSON = withObject "Model" $ \o ->
    Model
      -- Note that for a model serialized as JSON, the `id` must be present
      -- (this assumes that a serialized model always refers to one that exists
      -- in the DB already)
      <$> fmap Just (o .: "id")
      <*> o .: "name"
      <*> fmap (Oid . fromIntegral @Word64) (o .: "contents")
      <*> o .: "version"
      <*> o .: "card"
      <*> o .: "permissions"
      <*> o .:? "user"
{- ORMOLU_ENABLE -}

instance
  ( ToJSON uid,
    ToJSON gid,
    ToJSONKey gid
  ) =>
  ToJSON (Model uid gid Oid)
  where
  toJSON m =
    object
      [ "id" .= view #id m,
        "name" .= view #name m,
        "contents" .= view (#contents . to unOid) m,
        "version" .= view #version m,
        "card" .= view #card m,
        "permissions" .= view #permissions m,
        "user" .= view #user m
      ]
    where
      unOid :: Oid -> Word32
      unOid (Oid (CUInt x)) = x

data ModelPermissions
  = -- | The model can be read e.g. for running inference
    ReadModel
  | -- | The model can be updated e.g. during training. If write permissions
    -- are set for a group, read permissions are implicitly granted
    WriteModel
  deriving stock (Show, Eq, Generic)

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

data ModelDescription = ModelDescription
  { -- | General summary of model, cannot be empty
    summary :: NotNull Text,
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

newtype NotNull a = NotNull a
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance FromJSON (NotNull Text) where
  parseJSON = withText "NotNull Text" $ \case
    t
      | Text.null t -> fail "Text cannot be empty"
      | otherwise -> pure $ NotNull t

deriving newtype instance ToJSON (NotNull Text)

instance FromField (NotNull Text) where
  fromField f =
    maybe (returnError UnexpectedNull f mempty) $
      Text.Encoding.decodeUtf8 >>> \case
        t
          | Text.null t ->
              returnError ConversionFailed f "Expected non-empty text"
          | otherwise -> pure $ NotNull t

deriving newtype instance ToField (NotNull Text)

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
    -- The script of the parameter
    --
    -- For new parameters, this will be text
    --
    -- For existing inference params, this is the foreign key for the specific
    -- script in the `InferenceScript` table
    script :: s,
    model :: Id (Model uid gid Oid),
    inputs :: Vector (SingleOrMany p),
    outputs :: Vector (SingleOrMany p),
    user :: uid
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- We only want instances if the `script` is a `VCObjectHash`

deriving anyclass instance
  ( FromJSON p,
    Typeable p,
    FromField uid
  ) =>
  FromRow (InferenceParam uid gid p VCObjectHash)

instance
  ( ToJSON p,
    ToField uid
  ) =>
  ToRow (InferenceParam uid gid p VCObjectHash)
  where
  -- NOTE: Do not change the order of the field actions
  toRow ip =
    [ toField Default,
      ip ^. #script & toField,
      ip ^. #model & toField,
      ip ^. #inputs & toField,
      ip ^. #outputs & toField,
      ip ^. #user & toField
    ]

-- | A user, parameterized by the user and group types
data User uid gid = User
  { id :: uid,
    groups :: Vector gid
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow, NFData)

-- Bridge-related types

-- A value that can be used with Inferno. Note that this is significantly more
-- restrictive than Inferno's `Value` type, which cannot have sensible `ToJSON`
-- and `FromJSON` instances
data IValue
  = IText Text
  | IDouble Double
  | ITuple (IValue, IValue)
  | ITime EpochTime
  | IEmpty
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance FromJSON IValue where
  parseJSON = \case
    String t -> pure $ IText t
    Number n -> pure . IDouble $ toRealFloat n
    -- It's easier to just mark the time explicitly in an object,
    -- rather than try to deal with distinguishing times and doubles
    Object o -> ITime <$> o .: "time"
    Array a
      | [x, y] <- Vector.toList a ->
          fmap ITuple $
            (,) <$> parseJSON x <*> parseJSON y
      | Vector.null a -> pure IEmpty
    _ -> fail "Expected one of: string, double, empty array"

instance ToJSON IValue where
  toJSON = \case
    IDouble d -> toJSON d
    IText t -> toJSON t
    ITuple t -> toJSON t
    -- See above
    ITime t -> object ["time" .= t]
    IEmpty -> toJSON ()

data SingleOrMany a
  = Single a
  | Many (Vector a)
  deriving stock (Show, Eq, Generic, Functor)
  deriving anyclass (NFData)
  -- FIXME This is a pretty awful hack, but Postgres sub-arrays need to be
  -- the same length and writing a custom parser might be painful
  deriving (FromField, ToField) via Aeson (SingleOrMany a)

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
