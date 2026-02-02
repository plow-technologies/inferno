{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Inferno.ML.Types.Value.Compat where

import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Inferno.Types.Syntax (CustomType)
import Inferno.Utils.QQ.Module (moduleQuoter)
import Language.Haskell.TH.Quote (QuasiQuoter)

-- | Compatibility type for Inferno ML projects. This is intended to avoid
-- forcing a dependency on the @hasktorch@ package. For example, dummy types
-- can be provided (given correct class implementations) for tensors, models,
-- etc... for type-checking purposes purely
data MlValue tensor model mname x
  = VTensor tensor
  | VModel model
  | VModelName mname
  | VJson Aeson.Value
  | VSchema Schema
  | VExtended x

data Schema
  = Primitive SchemaPrimitive
  | Array (Vector Schema)
  | Object (Map Text Schema)
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON Schema where
  toJSON = \case
    Primitive p -> Aeson.toJSON p
    Array xs -> Aeson.toJSON xs
    Object o -> Aeson.toJSON o

-- | Primitive type for LLM response schemas
data SchemaPrimitive
  = String
  | Number
  | Bool
  deriving stock (Show, Eq, Generic)

-- This creates the meta type variables for the schema
instance Aeson.ToJSON SchemaPrimitive where
  toJSON = \case
    String -> "$string"
    Number -> "$number"
    Bool -> "$bool"

customTypes :: [CustomType]
customTypes =
  [ "tensor"
  , -- NOTE It seems that `modelName` needs to come before `model`,
    -- otherwise Inferno's parser fails??
    "modelName"
  , "model"
  , "write"
  , "json"
  , "schema"
  ]

mlQuoter :: QuasiQuoter
mlQuoter = moduleQuoter customTypes
