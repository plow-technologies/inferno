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
    pattern VExtended,
    ModelName (ModelName),
    enumDeviceHash,
    enumDTypeHash,
    -- Convenience re-export
    customTypes,
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import Inferno.ML.Types.Value.Compat (customTypes)
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
import Inferno.Types.Value (Value (VCustom))
import Inferno.Types.VersionControl (VCObjectHash, vcHash)
import Prettyprinter (Pretty (pretty), align)
import Torch
  ( ScriptModule,
    Tensor,
  )

type MlValue x = Compat.MlValue Tensor ScriptModule ModelName x

{-# COMPLETE VTensor, VModel, VModelName, VExtended #-}

pattern VTensor :: Tensor -> MlValue x
pattern VTensor t = Compat.VTensor t

pattern VModel :: ScriptModule -> MlValue x
pattern VModel m = Compat.VModel m

pattern VModelName :: ModelName -> MlValue x
pattern VModelName mn = Compat.VModelName mn

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
    VModel m -> align . pretty . Text.pack $ show m
    VModelName x -> align $ pretty x
    VExtended x -> align $ pretty x

instance ToValue (MlValue x) m Tensor where
  toValue = VCustom . VTensor

instance (Pretty x) => FromValue (MlValue x) m Tensor where
  fromValue = \case
    VCustom (VTensor t) -> pure t
    v -> couldNotCast v

instance ToValue (MlValue x) m ScriptModule where
  toValue = VCustom . VModel

instance (Pretty x) => FromValue (MlValue x) m ScriptModule where
  fromValue = \case
    VCustom (VModel t) -> pure t
    v -> couldNotCast v

instance ToValue (MlValue x) m ModelName where
  toValue = VCustom . VModelName

instance (Pretty x) => FromValue (MlValue x) m ModelName where
  fromValue = \case
    VCustom (VModelName t) -> pure t
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
