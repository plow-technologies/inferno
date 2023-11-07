{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
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
import Control.Applicative (asum)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (Array, Number, String),
    withArray,
    withScientific,
    withText,
  )
import Data.Char (toLower)
import Data.Int (Int64)
import Data.Scientific (Scientific, toRealFloat)
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
    :> StreamPost NewlineFraming JSON (ConduitT () (AsValue Scientific) IO ())

data AsValueTyped
  = Floats (AsValue Float)
  | Doubles (AsValue Double)
  | Int64s (AsValue Int64)
  deriving stock (Show, Eq, Generic)

toAsValueTyped :: AsValue Scientific -> AsValueTyped
toAsValueTyped = \case
  AsValue dt xs -> case dt of
    Float -> Floats . AsValue dt $ toRealFloat <$> xs
    Double -> Doubles . AsValue dt $ toRealFloat <$> xs
    Int64 -> Int64s . AsValue dt $ round <$> xs

data AsValue a = AsValue
  { dtype :: DType,
    values :: Dims a
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

catAsValues :: AsValue a -> AsValue a -> Maybe (AsValue a)
catAsValues (AsValue dt1 x) = \case
  AsValue dt2 y
    | dt1 == dt2 -> case (x, y) of
        (Ones xs, Ones ys) -> Just . AsValue dt1 . Ones $ xs <> ys
        (Twos xs, Twos ys) -> Just . AsValue dt1 . Twos $ xs <> ys
        (Threes xs, Threes ys) -> Just . AsValue dt1 . Threes $ xs <> ys
        (Fours xs, Fours ys) -> Just . AsValue dt1 . Fours $ xs <> ys
        _ -> Nothing
    | otherwise -> Nothing

data Dims a
  = Ones [a]
  | Twos [[a]]
  | Threes [[[a]]]
  | Fours [[[[a]]]]
  deriving stock (Show, Eq, Generic, Functor)

instance FromJSON a => FromJSON (Dims a) where
  parseJSON = withArray "Dims" $ \a ->
    asum
      [ Ones <$> parseJSON (Array a),
        Twos <$> parseJSON (Array a),
        Threes <$> parseJSON (Array a),
        Fours <$> parseJSON (Array a)
      ]

instance ToJSON a => ToJSON (Dims a) where
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

data Dim
  = One
  | Two
  | Three
  | Four
  deriving stock (Show, Eq, Ord, Generic)

instance FromJSON Dim where
  parseJSON = withScientific "Dim" $ \case
    1 -> pure One
    2 -> pure Two
    3 -> pure Three
    4 -> pure Four
    n -> fail $ unwords ["Dim out of range:", show n]

instance ToJSON Dim where
  toJSON = \case
    One -> Number 1
    Two -> Number 2
    Three -> Number 3
    Four -> Number 4

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
