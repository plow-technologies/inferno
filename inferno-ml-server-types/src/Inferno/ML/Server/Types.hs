{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Inferno.ML.Server.Types where

import Conduit (ConduitT)
import Data.Aeson
  ( Encoding,
    FromJSON (parseJSON),
    Object,
    ToJSON (toEncoding, toJSON),
    Value (Number, String),
    pairs,
    withObject,
    withScientific,
    withText,
    (.:),
    (.=),
  )
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import Data.Int (Int64)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import Database.PostgreSQL.Simple
  ( FromRow,
    ToRow,
  )
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.LargeObjects (Oid)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.Generics (Generic)
import Servant
  ( JSON,
    NewlineFraming,
    ReqBody,
    StreamPost,
    (:>),
  )
import Servant.Conduit ()

type InfernoMlServerAPI uid gid =
  -- Evaluate an inference script. The script must evaluate to a tensor, which
  -- will then be converted to an array, which will subsequently be streamed in chunks
  --
  -- NOTE: The endpoint streams back individual chunks of the list with the same
  -- depth as the converted tensor. The resulting lists can then be concatenated
  -- to recover the original list with the correct dimensions
  "inference"
    :> ReqBody '[JSON] (InferenceRequest uid gid)
    :> StreamPost NewlineFraming JSON (ConduitT () AsValue IO ())

data AsValue
  = AsValue1 DType [DValue]
  | AsValue2 DType [[DValue]]
  | AsValue3 DType [[[DValue]]]
  | AsValue4 DType [[[[DValue]]]]
  deriving stock (Show, Eq, Generic)

instance ToJSON AsValue where
  toEncoding = \case
    AsValue1 dt xs -> mkPairs One dt xs
    AsValue2 dt xs -> mkPairs Two dt xs
    AsValue3 dt xs -> mkPairs Three dt xs
    AsValue4 dt xs -> mkPairs Four dt xs
    where
      mkPairs :: ToJSON a => Dim -> DType -> a -> Encoding
      mkPairs dim dt xs =
        pairs $
          mconcat
            [ "dim" .= dim,
              "dtype" .= dt,
              "chunk" .= xs
            ]

instance FromJSON AsValue where
  parseJSON = withObject "AsValue" $ \o ->
    ((,) <$> o .: "dim" <*> o .: "dtype") >>= \case
      (One, dt) -> AsValue1 dt <$> getChunk1 o dt
      (Two, dt) -> AsValue2 dt <$> getChunk2 o dt
      (Three, dt) -> AsValue3 dt <$> getChunk3 o dt
      (Four, dt) -> AsValue4 dt <$> getChunk4 o dt
    where
      getChunk1 :: Object -> DType -> Parser [DValue]
      getChunk1 o = \case
        Float -> fmap DFloat <$> o .: "chunk"
        Double -> fmap DDouble <$> o .: "chunk"
        Int64 -> fmap DInt64 <$> o .: "chunk"

      getChunk2 :: Object -> DType -> Parser [[DValue]]
      getChunk2 o = \case
        Float -> (fmap . fmap) DFloat <$> o .: "chunk"
        Double -> (fmap . fmap) DDouble <$> o .: "chunk"
        Int64 -> (fmap . fmap) DInt64 <$> o .: "chunk"

      getChunk3 :: Object -> DType -> Parser [[[DValue]]]
      getChunk3 o = \case
        Float -> (fmap . fmap . fmap) DFloat <$> o .: "chunk"
        Double -> (fmap . fmap . fmap) DDouble <$> o .: "chunk"
        Int64 -> (fmap . fmap . fmap) DInt64 <$> o .: "chunk"

      getChunk4 :: Object -> DType -> Parser [[[[DValue]]]]
      getChunk4 o = \case
        Float -> (fmap . fmap . fmap . fmap) DFloat <$> o .: "chunk"
        Double -> (fmap . fmap . fmap . fmap) DDouble <$> o .: "chunk"
        Int64 -> (fmap . fmap . fmap . fmap) DInt64 <$> o .: "chunk"

-- | Supported tensor datatypes.
data DType
  = Int64
  | Float
  | Double
  deriving stock (Show, Eq, Generic)

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

data DValue
  = DInt64 Int64
  | DFloat Float
  | DDouble Double
  deriving stock (Show, Eq, Generic)

instance FromJSON DValue where
  parseJSON = undefined

instance ToJSON DValue where
  toJSON = \case
    DInt64 i -> toJSON i
    DFloat f -> toJSON f
    DDouble d -> toJSON d

-- | Tensor dimensions. Currently, @inferno-ml@ supports converting between
-- tensors and Haskell lists for up to four dimensions. This is included in the
-- response when running an inference parameter.
data Dim
  = One
  | Two
  | Three
  | Four
  deriving stock
    ( Show,
      Eq,
      Ord,
      Bounded,
      Generic
    )

instance Enum Dim where
  toEnum = \case
    1 -> One
    2 -> Two
    3 -> Three
    4 -> Four
    _ -> error "Dimension out of bounds (1, 4)"

  fromEnum = \case
    One -> 1
    Two -> 2
    Three -> 3
    Four -> 4

instance FromJSON Dim where
  parseJSON = withScientific "Dim" $ \case
    1 -> pure One
    2 -> pure Two
    3 -> pure Three
    4 -> pure Four
    n -> fail $ unwords ["Invalid dimension:", show n]

instance ToJSON Dim where
  toJSON = Number . fromIntegral . fromEnum

-- | A request to run an inference parameter
data InferenceRequest uid gid = InferenceRequest
  { parameter :: Id (InferenceParam uid gid),
    model :: Id (Model uid gid)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | The ID of a database entity
newtype Id a = Id Int64
  deriving stock (Show, Generic)
  deriving newtype
    ( Eq,
      FromField,
      ToField,
      FromJSON,
      ToJSON
    )

-- Row of the model table, parameterized by the user and group type
data Model uid gid = Model
  { id :: Maybe (Id (Model uid gid)),
    name :: Text,
    -- The actual contents of the model
    contents :: Oid,
    version :: Text,
    -- The groups able to access the model
    groups :: Vector gid,
    -- Not currently used
    user :: Maybe uid
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromRow, ToRow)

-- | Row of the inference parameter table, parameterized by the user type
data InferenceParam uid gid = InferenceParam
  { id :: Maybe (Id (InferenceParam uid gid)),
    -- FIXME Better type
    script :: Script,
    model :: Id (Model uid gid),
    user :: uid
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromRow, ToRow)

-- | A user, parameterized by the user and group types
data User uid gid = User
  { id :: uid,
    groups :: Vector gid
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow)

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
