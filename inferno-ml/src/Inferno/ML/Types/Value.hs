{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

module Inferno.ML.Types.Value
  ( MlValue,
    pattern VTensor,
    pattern VModel,
    pattern VModelName,
    pattern VJson,
    pattern VExtended,
    ModelName (ModelName),
    enumDeviceHash,
    enumDTypeHash,
    -- Convenience re-exports
    module M,
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson.Encode.Pretty
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Lazy.Builder
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import Inferno.ML.Types.Compat
  ( BedrockConfig (BedrockConfig),
    ModelConfig (Bedrock, TorchScript),
    Temperature,
    mkTemperature,
  )
import Inferno.ML.Types.Compat as M
  ( BedrockConfig (BedrockConfig),
    ModelConfig (Bedrock, TorchScript),
    Temperature (Temperature),
    mkTemperature,
  )
import Inferno.ML.Types.Value.Compat as M (customTypes)
import qualified Inferno.ML.Types.Value.Compat as Compat
import Inferno.Module (BuiltinEnumHash (BuiltinEnumHash))
import Inferno.Module.Cast
  ( FromValue (fromValue),
    ToValue (toValue),
    couldNotCast,
  )
import Inferno.Types.Syntax
  ( BaseType (TEnum),
    Ident,
    ImplType (ImplType),
    InfernoType (TBase),
    TCScheme (ForallTC),
  )
import Inferno.Types.Value (Value (VCustom, VText, VTuple))
import Inferno.Types.VersionControl (VCObjectHash, vcHash)
import Prettyprinter (Pretty (pretty), align)
import Torch
  ( ScriptModule,
    Tensor,
  )

type MlValue x = Compat.MlValue Tensor (ModelConfig ScriptModule) ModelName x

{-# COMPLETE VTensor, VModel, VModelName, VJson, VExtended #-}

pattern VTensor :: Tensor -> MlValue x
pattern VTensor t = Compat.VTensor t

pattern VModel :: ModelConfig ScriptModule -> MlValue x
pattern VModel m = Compat.VModel m

pattern VModelName :: ModelName -> MlValue x
pattern VModelName mn = Compat.VModelName mn

pattern VJson :: Aeson.Value -> MlValue x
pattern VJson j = Compat.VJson j

pattern VExtended :: x -> MlValue x
pattern VExtended x = Compat.VExtended x

-- | The UUID of a model version; used to identify models for caching and loading
newtype ModelName = ModelName UUID
  deriving stock (Show, Generic)
  deriving newtype (Eq)

instance Pretty ModelName where
  pretty (ModelName uuid) = pretty $ UUID.toText uuid

instance (Eq x) => Eq (MlValue x) where
  VTensor t1 == VTensor t2 = t1 == t2
  VModelName n1 == VModelName n2 = n1 == n2
  VExtended x == VExtended y = x == y
  _ == _ = False

instance (Pretty x) => Pretty (MlValue x) where
  pretty = \case
    VTensor t -> align . pretty . Text.pack $ show t
    VModel (TorchScript m) -> align . pretty . Text.pack $ show m
    VModel (Bedrock bc) -> align . pretty . Text.pack $ show bc
    VModelName x -> align $ pretty x
    VJson j ->
      align
        . pretty
        . Text.Lazy.toStrict
        . Text.Lazy.Builder.toLazyText
        . Aeson.Encode.Pretty.encodePrettyToTextBuilder'
          Aeson.Encode.Pretty.defConfig
            { Aeson.Encode.Pretty.confIndent = Aeson.Encode.Pretty.Spaces 2
            }
        $ j
    VExtended x -> align $ pretty x

instance ToValue (MlValue x) m Tensor where
  toValue = VCustom . VTensor

instance (Pretty x) => FromValue (MlValue x) m Tensor where
  fromValue = \case
    VCustom (VTensor t) -> pure t
    v -> couldNotCast v

instance ToValue (MlValue x) m (ModelConfig ScriptModule) where
  toValue = VCustom . VModel

instance (Pretty x) => FromValue (MlValue x) m (ModelConfig ScriptModule) where
  fromValue = \case
    VCustom (VModel mc) -> pure mc
    v -> couldNotCast v

instance ToValue (MlValue x) m ScriptModule where
  toValue = VCustom . VModel . TorchScript

instance (Pretty x) => FromValue (MlValue x) m ScriptModule where
  fromValue = \case
    VCustom (VModel (TorchScript sm)) -> pure sm
    v -> couldNotCast v

instance ToValue (MlValue x) m BedrockConfig where
  toValue = VCustom . VModel . Bedrock

instance (Pretty x) => FromValue (MlValue x) m BedrockConfig where
  fromValue = \case
    VCustom (VModel (Bedrock bc)) -> pure bc
    v -> couldNotCast v

instance ToValue (MlValue x) m ModelName where
  toValue = VCustom . VModelName

instance (Pretty x) => FromValue (MlValue x) m ModelName where
  fromValue = \case
    VCustom (VModelName t) -> pure t
    v -> couldNotCast v

instance ToValue (MlValue x) m Aeson.Value where
  toValue = VCustom . VJson

instance (Pretty x) => FromValue (MlValue x) m Aeson.Value where
  fromValue = \case
    VCustom (VJson j) -> pure j
    v -> couldNotCast v

-- Helpers for unpacking Aeson `KeyMap`s into lists of tuples

instance ToValue (MlValue x) m (Text, Aeson.Value) where
  toValue (k, v) = VTuple [toValue k, toValue v]

instance (Pretty x) => FromValue (MlValue x) m (Text, Aeson.Value) where
  fromValue = \case
    VTuple [VText k, VCustom (VJson v)] -> pure (k, v)
    v -> couldNotCast v

-- We need a hash for the `device` enum in Inferno in order for functions
-- to evaluate to an Inferno `device`
enumDeviceHash :: VCObjectHash
enumDeviceHash = hashEnum "device" ["cpu", "cuda"]

-- We need a hash for the `dtype` enum in Inferno in order for functions
-- to evaluate to an Inferno `dtype`
enumDTypeHash :: VCObjectHash
enumDTypeHash = hashEnum "dtype" ["int", "float", "double", "bool"]

hashEnum :: Text -> [Ident] -> VCObjectHash
hashEnum name =
  vcHash
    . BuiltinEnumHash
    . ForallTC [] Set.empty
    . ImplType Map.empty
    . TBase
    . TEnum name
    . Set.fromList
