{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors #-}

module Inferno.ML.Types.Compat where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Sum type representing different model configuration types. The type
-- parameter represents the contents of a TorchScript model; for DB storage
-- this is an `Oid`, while for runtime evaluation this would be a `ScriptModule`
data ModelConfig a
  = TorchScript a
  | Bedrock BedrockConfig
  deriving stock (Show, Eq, Generic, Functor)

-- | Configuration for Bedrock-based models
data BedrockConfig = BedrockConfig
  { modelId :: Text
  -- ^ The *Bedrock* model identifier (e.g., "anthropic.claude-3-5-sonnet-20241022-v2:0")
  , temperature :: Temperature
  -- ^ Optional temperature parameter for generation; defaults to 0.2
  , topP :: TopP
  -- ^ Optional top-p parameter for generation; defaults to 0.9
  }
  deriving stock (Show, Eq, Generic)

-- | Normalized parameters for LLM inference (i.e. in range 0.0 to 1.0).
--
-- Validated during construction and deserialization to ensure it falls within
-- the valid range.
newtype Normalized = Normalized Float
  deriving stock (Show, Generic)
  deriving newtype (Eq)

-- | Smart constructor for 'Normalized' that validates the range.
mkNormalized :: Float -> Maybe Normalized
mkNormalized f
  | f >= 0.0 && f <= 1.0 = Just $ Normalized f
  | otherwise = Nothing

-- | Temperature parameter for LLM generation (0.0 to 1.0).
--
-- Validated during construction and deserialization to ensure it falls within
-- the valid range.
newtype Temperature = Temperature Normalized
  deriving stock (Show, Generic)
  deriving newtype (Eq)

-- | Smart constructor for 'Temperature' that validates the range.
mkTemperature :: Float -> Maybe Temperature
mkTemperature = fmap Temperature . mkNormalized

-- | Temperature parameter for LLM generation (0.0 to 1.0).
--
-- Validated during construction and deserialization to ensure it falls within
-- the valid range.
newtype TopP = TopP Normalized
  deriving stock (Show, Generic)
  deriving newtype (Eq)

-- | Smart constructor for 'TopP' that validates the range.
mkTopP :: Float -> Maybe TopP
mkTopP = fmap TopP . mkNormalized
