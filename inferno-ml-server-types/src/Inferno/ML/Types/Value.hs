{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Inferno.ML.Types.Value where

import qualified Data.Text as Text
import GHC.Generics (Generic)
import Inferno.Module.Cast (FromValue (..), ToValue (..), couldNotCast)
import Inferno.Types.Syntax (CustomType)
import Inferno.Types.Value (Value (VCustom))
import Inferno.Utils.QQ.Module (moduleQuoter)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Prettyprinter (Pretty (pretty), align)
import qualified Torch as T

-- | The name of a serialized model, e.g. @model.ts.pt@
newtype ModelName = ModelName FilePath
  deriving stock (Show, Generic)
  deriving newtype (Eq, Pretty)

data MlValue x
  = VTensor T.Tensor
  | VModel T.ScriptModule
  | VModelName ModelName
  | VExtended x

instance (Eq x) => Eq (MlValue x) where
  VTensor t1 == VTensor t2 = t1 == t2
  VExtended x == VExtended y = x == y
  _ == _ = False

instance (Pretty x) => Pretty (MlValue x) where
  pretty = \case
    VTensor t -> align (pretty $ Text.pack $ show t)
    VModel m -> align (pretty $ Text.pack $ show m)
    VModelName x -> align $ pretty x
    VExtended x -> align $ pretty x

instance ToValue (MlValue x) m T.Tensor where
  toValue = VCustom . VTensor

instance (Pretty x) => FromValue (MlValue x) m T.Tensor where
  fromValue (VCustom (VTensor t)) = pure t
  fromValue v = couldNotCast v

instance ToValue (MlValue x) m T.ScriptModule where
  toValue = VCustom . VModel

instance (Pretty x) => FromValue (MlValue x) m T.ScriptModule where
  fromValue (VCustom (VModel t)) = pure t
  fromValue v = couldNotCast v

instance ToValue (MlValue x) m ModelName where
  toValue = VCustom . VModelName

instance (Pretty x) => FromValue (MlValue x) m ModelName where
  fromValue = \case
    VCustom (VModelName t) -> pure t
    v -> couldNotCast v

customTypes :: [CustomType]
customTypes =
  [ "tensor"
  , -- NOTE It seems that `modelName` needs to come before `model`,
    -- otherwise Inferno's parser fails??
    "modelName"
  , "model"
  , "write"
  ]

mlQuoter :: QuasiQuoter
mlQuoter = moduleQuoter customTypes
