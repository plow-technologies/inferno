{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}

module Inferno.ML.Server.Module.Types where

import Control.DeepSeq (NFData)
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bits (countLeadingZeros)
import Data.Generics.Labels ()
import Data.Int (Int64)
import qualified Data.Vector as Vector
import Data.Word (Word8)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.Generics (Generic)
import Inferno.Eval (TermEnv)
import Inferno.Eval.Error (EvalError (RuntimeError))
import Inferno.ML.Types.Value (MlValue (VExtended))
import Inferno.Module.Cast
  ( FromValue (fromValue),
    ToValue (toValue),
    couldNotCast,
  )
import Inferno.Types.Value
  ( ImplEnvM,
    Value
      ( VArray,
        VCustom,
        VDouble,
        VEmpty,
        VEpochTime,
        VInt,
        VText,
        VTuple,
        VWord16,
        VWord32,
        VWord64
      ),
  )
import Inferno.Types.VersionControl (VCObjectHash)
import Prettyprinter (Pretty (pretty), cat, (<+>))
import System.Posix.Types (EpochTime)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)
import "inferno-ml-server-types" Inferno.ML.Server.Types

-- | Custom type for bridge prelude
data BridgeValue
  = VResolution InverseResolution
  | VSeries PID
  | VWrite (PID, [(IValue, EpochTime)])
  deriving stock (Generic)

instance Eq BridgeValue where
  VResolution r1 == VResolution r2 = r1 == r2
  VSeries v1 == VSeries v2 = v1 == v2
  VWrite w1 == VWrite w2 = w1 == w2
  _ == _ = False

instance Pretty BridgeValue where
  pretty = \case
    VSeries p -> cat ["<<", "series" <+> pretty p, ">>"]
    VResolution e -> pretty @Int $ 2 ^ e
    VWrite (p, vs) -> "Write" <+> pretty p <> ":" <+> pretty (show vs)

-- | Unique ID for pollable data point (for the data source that can be
-- queried using the bridge)
newtype PID = PID Int
  deriving stock (Show, Generic)
  deriving newtype
    ( Eq
    , Ord
    , ToJSON
    , FromJSON
    , FromHttpApiData
    , ToHttpApiData
    , Pretty
    , FromField
    , ToField
    , NFData
    )

instance ToValue (MlValue BridgeValue) m PID where
  toValue = VCustom . VExtended . VSeries

instance FromValue (MlValue BridgeValue) m PID where
  fromValue = \case
    VCustom (VExtended (VSeries p)) -> pure p
    v -> couldNotCast v

newtype InverseResolution = InverseResolution Word8
  deriving stock (Show, Generic)
  deriving newtype
    ( Eq
    , Ord
    , Num
    , Real
    , Enum
    , Integral
    , NFData
    )

instance ToValue (MlValue BridgeValue) m InverseResolution where
  toValue = VCustom . VExtended . VResolution

instance FromValue (MlValue BridgeValue) m InverseResolution where
  fromValue = \case
    VCustom (VExtended (VResolution r)) -> pure r
    v -> couldNotCast v

data BridgeFuns m = BridgeFuns
  { valueAt :: BridgeV m
  , latestValueAndTimeBefore :: BridgeV m
  , latestValueAndTime :: BridgeV m
  , valuesBetween :: BridgeV m
  }
  deriving stock (Generic)

fromIValue :: IValue -> Value v m
fromIValue = \case
  IText t -> VText t
  IInt i -> VInt i
  IWord16 w -> VWord16 w
  IWord32 w -> VWord32 w
  IWord64 w -> VWord64 w
  IDouble d -> VDouble d
  ITime t -> VEpochTime t
  IEmpty -> VEmpty
  ITuple (x, y) -> VTuple [fromIValue x, fromIValue y]
  IArray v -> VArray $ Vector.toList $ fromIValue <$> v

toIValue :: (MonadThrow f) => Value custom m -> f IValue
toIValue = \case
  VText t -> pure $ IText t
  VInt i -> pure $ IInt i
  VWord16 w -> pure $ IWord16 w
  VWord32 w -> pure $ IWord32 w
  VWord64 w -> pure $ IWord64 w
  VDouble d -> pure $ IDouble d
  VEpochTime t -> pure $ ITime t
  VEmpty -> pure IEmpty
  VTuple [x, y] -> curry ITuple <$> toIValue x <*> toIValue y
  VArray vs -> IArray . Vector.fromList <$> traverse toIValue vs
  _ -> throwM $ RuntimeError "toIValue: got an unsupported value type"

toResolution :: Int64 -> InverseResolution
toResolution =
  InverseResolution
    . ((63 -) . fromIntegral . countLeadingZeros)
    . max 1

resolutionToInt :: InverseResolution -> Int64
resolutionToInt (InverseResolution w) = 2 ^ w

type BridgeImplM m = ImplEnvM m BridgeMlValue (BridgeV m)

type BridgeV m = Value BridgeMlValue (ImplEnvM m BridgeMlValue)

type BridgeTermEnv m =
  TermEnv VCObjectHash BridgeMlValue (ImplEnvM m BridgeMlValue) ()

type BridgeMlValue = MlValue BridgeValue
