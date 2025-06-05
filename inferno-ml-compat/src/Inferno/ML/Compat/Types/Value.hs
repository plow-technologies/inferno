{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Inferno.ML.Compat.Types.Value where

import Data.Data (Typeable)
import qualified Data.Text as Text
import Inferno.Module.Cast
  ( FromValue (fromValue),
    ToValue (toValue),
    couldNotCast,
  )
import Inferno.Types.Syntax (CustomType)
import Inferno.Types.Value (Value (VCustom))
import Inferno.Utils.QQ.Module (moduleQuoter)
import Language.Haskell.TH.Quote (QuasiQuoter)
import Prettyprinter (Pretty (pretty), align)

-- | Compatibility type for Inferno ML projects. This is intended to avoid
-- forcing a dependency on the @hasktorch@ package. For example, dummy types
-- can be provided (given correct class implementations) for tensors, models,
-- etc... for type-checking purposes purely
data MlValue tensor model mname x
  = VTensor tensor
  | VModel model
  | VModelName mname
  | VExtended x

instance
  ( Eq x
  , Eq tensor
  , Eq mname
  ) =>
  Eq (MlValue tensor model mname x)
  where
  VTensor t1 == VTensor t2 = t1 == t2
  VModelName n1 == VModelName n2 = n1 == n2
  VExtended x == VExtended y = x == y
  _ == _ = False

instance
  ( Pretty x
  , Show tensor
  , Show model
  , Pretty mname
  ) =>
  Pretty (MlValue tensor model mname x)
  where
  pretty = \case
    VTensor t -> align . pretty . Text.pack $ show t
    VModel m -> align . pretty . Text.pack $ show m
    VModelName x -> align $ pretty x
    VExtended x -> align $ pretty x

instance ToValue (MlValue tensor model mname x) m tensor where
  toValue = VCustom . VTensor

instance
  ( Show tensor
  , Show model
  , Show mname
  , Pretty tensor
  , Pretty model
  , Pretty mname
  , Pretty x
  , Typeable tensor
  ) =>
  FromValue (MlValue tensor model mname x) m tensor
  where
  fromValue = \case
    VCustom (VTensor t) -> pure t
    v -> couldNotCast v

instance ToValue (MlValue tensor model mname x) m model where
  toValue = VCustom . VModel

instance
  ( Show tensor
  , Show model
  , Show mname
  , Pretty tensor
  , Pretty model
  , Pretty mname
  , Pretty x
  , Typeable model
  ) =>
  FromValue (MlValue tensor model mname x) m model
  where
  fromValue = \case
    VCustom (VModel m) -> pure m
    v -> couldNotCast v

instance ToValue (MlValue tensor model mname x) m mname where
  toValue = VCustom . VModelName

instance
  ( Show tensor
  , Show model
  , Show mname
  , Pretty tensor
  , Pretty model
  , Pretty mname
  , Pretty x
  , Typeable mname
  ) =>
  FromValue (MlValue tensor model mname x) m mname
  where
  fromValue = \case
    VCustom (VModelName n) -> pure n
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
