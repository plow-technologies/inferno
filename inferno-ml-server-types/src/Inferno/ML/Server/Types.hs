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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Inferno.ML.Server.Types where

import Conduit (ConduitT)
import Data.Aeson
  ( FromJSON (parseJSON),
    Object,
    ToJSON (toJSON),
    Value (Number, String),
    object,
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
import Web.HttpApiData
  ( FromHttpApiData (parseUrlPiece),
    ToHttpApiData (toUrlPiece),
  )

type InfernoMlServerAPI uid gid =
  -- Evaluate an inference script. The script must evaluate to a tensor, which
  -- will then be converted to an array, whose elements will be subsequently
  -- streamed as chunks
  "inference"
    :> ReqBody '[JSON] (InferenceRequest uid gid)
    :> StreamPost NewlineFraming JSON (ConduitT () SomeChunk IO ())

data SomeValue where
  SomeValue :: Chunkable a => AsValue a -> SomeValue

-- | We need to be able to support chunk sizes based on the dimensions of the input
-- tensor, which may be nested, and of varying @dtype@s
data SomeChunk where
  -- Note that this encodes the `dtype` and `dim` redundantly, but AFAICT there's
  -- no other way to include this information using `NewlineFraming` with `JSON`,
  -- since that framing strategy apparently doesn't support adding response headers
  -- (where the `dtype` and `dim` could also be added); also, headers are not
  -- available in `MimeUnrender` implementations, so decoding would be an issue
  SomeChunk :: forall a. Chunkable a => Dim -> DType -> a -> SomeChunk

type Chunkable a = (Show a, ToJSON a, FromJSON a)

deriving stock instance Show SomeChunk

instance FromJSON SomeChunk where
  parseJSON = withObject "SomeChunk" $ \o ->
    someChunkP o =<< ((,) <$> o .: "dim" <*> o .: "dtype")
    where
      -- Each chunk is one element of the list from the original n-dimension
      -- tensor (up to four dimensions)
      someChunkP :: Object -> (Dim, DType) -> Parser SomeChunk
      someChunkP o = \case
        x@(One, Float) -> mkSomeChunk x <$> getChunk @Float
        x@(Two, Float) -> mkSomeChunk x <$> getChunk @[Float]
        x@(Three, Float) -> mkSomeChunk x <$> getChunk @[[Float]]
        x@(Four, Float) -> mkSomeChunk x <$> getChunk @[[[Float]]]
        x@(One, Double) -> mkSomeChunk x <$> getChunk @Double
        x@(Two, Double) -> mkSomeChunk x <$> getChunk @[Double]
        x@(Three, Double) -> mkSomeChunk x <$> getChunk @[[Double]]
        x@(Four, Double) -> mkSomeChunk x <$> getChunk @[[[Double]]]
        x@(One, Int64) -> mkSomeChunk x <$> getChunk @Int64
        x@(Two, Int64) -> mkSomeChunk x <$> getChunk @[Int64]
        x@(Three, Int64) -> mkSomeChunk x <$> getChunk @[[Int64]]
        x@(Four, Int64) -> mkSomeChunk x <$> getChunk @[[[Int64]]]
        where
          mkSomeChunk :: Chunkable a => (Dim, DType) -> a -> SomeChunk
          mkSomeChunk = uncurry SomeChunk

          getChunk :: FromJSON a => Parser a
          getChunk = o .: "chunk"

instance ToJSON SomeChunk where
  toJSON (SomeChunk dim dtype x) =
    object
      [ "dtype" .= dtype,
        "dim" .= dim,
        "chunk" .= x
      ]

-- | Representation of tensor, parameterized by its datatype, up to four dimensions
-- (see 'Dim')
data AsValue a where
  AsValue1 :: [a] -> AsValue [a]
  AsValue2 :: [[a]] -> AsValue [[a]]
  AsValue3 :: [[[a]]] -> AsValue [[[a]]]
  AsValue4 :: [[[[a]]]] -> AsValue [[[[a]]]]

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

instance ToHttpApiData DType where
  toUrlPiece = \case
    Int64 -> "int"
    d -> Text.pack $ toLower <$> show d

instance FromHttpApiData DType where
  parseUrlPiece = \case
    "int" -> pure Int64
    "float" -> pure Float
    "double" -> pure Double
    d -> Left . Text.pack $ unwords ["Invalid dtype:", show d]

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

instance ToHttpApiData Dim where
  toUrlPiece = Text.pack . show . fromEnum

instance FromHttpApiData Dim where
  parseUrlPiece = \case
    "1" -> pure One
    "2" -> pure Two
    "3" -> pure Three
    "4" -> pure Four
    n -> Left . Text.pack $ unwords ["Invalid dimension:", show n]

vdim :: AsValue a -> Dim
vdim = \case
  AsValue1 _ -> One
  AsValue2 _ -> Two
  AsValue3 _ -> Three
  AsValue4 _ -> Four

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
