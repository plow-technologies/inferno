{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}

module Inferno.ML.Server.Module.Types where

import Control.Monad.Catch (MonadThrow (throwM))
import Data.Bits (countLeadingZeros)
import Data.Generics.Labels ()
import Data.Int (Int64)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Inferno.Eval (TermEnv)
import Inferno.Eval.Error (EvalError (RuntimeError))
import Inferno.ML.Types.Value (MlValue)
import Inferno.Types.Value
  ( ImplEnvM,
    Value (VArray, VDouble, VEmpty, VEpochTime, VText, VTuple),
  )
import Inferno.Types.VersionControl (VCObjectHash)
import "inferno-ml-server-types" Inferno.ML.Server.Types
  ( BridgeValue,
    IValue (IArray, IDouble, IEmpty, IText, ITime, ITuple),
    InverseResolution (..),
  )

data BridgeFuns m = BridgeFuns
  { valueAt :: BridgeV m
  , latestValueAndTimeBefore :: BridgeV m
  , latestValueAndTime :: BridgeV m
  , valuesBetween :: BridgeV m
  }
  deriving stock (Generic)

fromIValue :: IValue -> Value v m
fromIValue = \case
  IDouble d -> VDouble d
  IText t -> VText t
  ITime t -> VEpochTime t
  ITuple (x, y) -> VTuple [fromIValue x, fromIValue y]
  IEmpty -> VEmpty
  IArray v -> VArray $ Vector.toList $ fromIValue <$> v

toIValue :: (MonadThrow f) => Value custom m -> f IValue
toIValue = \case
  VText t -> pure $ IText t
  VDouble d -> pure $ IDouble d
  VEpochTime t -> pure $ ITime t
  VTuple [x, y] -> curry ITuple <$> toIValue x <*> toIValue y
  VEmpty -> pure IEmpty
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
