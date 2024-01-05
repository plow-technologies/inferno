{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Inferno.ML.Server.Types where

import Conduit (ConduitT)
import Control.Applicative (asum)
import Control.DeepSeq (NFData (rnf), rwhnf)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toEncoding, toJSON),
    Value (Array, Number, Object, String),
    object,
    pairs,
    withArray,
    withScientific,
    withText,
    (.:),
    (.=),
  )
import Data.Char (toLower)
import Data.IP (IPv4)
import Data.Int (Int64)
import Data.Scientific (Scientific, toRealFloat)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word64)
import Database.PostgreSQL.Simple
  ( FromRow,
    ToRow,
  )
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.LargeObjects (Oid)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.Generics (Generic)
import Inferno.ML.Server.Types.Orphans ()
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
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

type InfernoMlServerAPI uid gid p =
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
      :> Capture "id" (Id (InferenceParam uid gid p))
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

instance NFData (Model uid gid) where
  rnf = rwhnf

-- | Row of the inference parameter table, parameterized by the user type
data InferenceParam uid gid p = InferenceParam
  { id :: Maybe (Id (InferenceParam uid gid p)),
    -- FIXME Better type
    script :: Script,
    model :: Id (Model uid gid),
    input :: Maybe p,
    user :: uid
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromRow, ToRow, NFData)

-- | A user, parameterized by the user and group types
data User uid gid = User
  { id :: uid,
    groups :: Vector gid
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow, NFData)

newtype Script = Script Text
  deriving stock (Show, Generic)
  deriving newtype
    ( Eq,
      FromJSON,
      ToJSON,
      IsString,
      FromField,
      ToField,
      NFData
    )

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
