{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Inferno.ML.Types.Value.Compat where

import qualified Data.Aeson as Aeson
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import Data.Text.Lazy.Builder
  ( Builder
  , toLazyText
  , fromText
  , singleton
  )
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

-- | A schema containing type and shape information for LLM responses. This
-- can be a composite type or a single scalar primitive
data Schema
  = -- | For either using a 'Primitive' as a schema alone, or as part of a
    -- composite
    Primitive SchemaPrimitive
  | -- | Note: since Inferno arrays a homoegeneous, we only allow the @Array@
    -- case to hold a single schema. E.g. @[$number]@
    Array Schema
  | Object (Map Text Schema)
  deriving stock (Show, Eq, Generic)

-- | This uses a @Text@-based builder instead of Aeson to create the schema. We
-- don't want type meta-variables quoted, for example. E.g. we want @$string@,
-- not @"$string"@. This is NOT meant to be parseable as JSON, it's a textual
-- description of a 'Schema' that resembles JSON
renderSchema :: Schema -> Text
renderSchema = Text.Lazy.toStrict . toLazyText . render
  where
    render :: Schema -> Builder
    render = \case
      Primitive p -> case p of
        String -> fromText "$string"
        Number -> fromText "$number"
        Bool -> fromText "$bool"
      Array a -> mconcat [singleton '[', render a, singleton ']']
      Object o ->
        mconcat
          [ singleton '{'
          , commaSep $ Map.toList o <&> \(k, v) ->
              mconcat
                [ -- Creates the "keys", formatted similarly to JSON keys
                  -- (unlike the primitive meta-variables)
                  mconcat
                  [ singleton '"'
                  , fromText k
                  , singleton '"'
                  , singleton ':'
                  , singleton ' '
                  ]
                , render v
                ]
          , singleton '}'
          ]
        where
          commaSep :: [Builder] -> Builder
          commaSep [] = mempty
          commaSep (b : bs) =
            mconcat
              [ b
              , foldMap (fromText ", " <>) bs
              ]

-- | Primitive type for LLM response schemas
data SchemaPrimitive
  = String
  | Number
  | Bool
  deriving stock (Show, Eq, Generic)


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
