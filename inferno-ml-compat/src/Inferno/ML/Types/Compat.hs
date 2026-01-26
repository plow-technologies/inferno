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
  -- ^ The Bedrock model identifier (e.g., "anthropic.claude-3-5-sonnet-20241022-v2:0")
  , region :: Maybe Text
  -- ^ AWS region for the Bedrock service
  , temperature :: Temperature
  -- ^ Temperature parameter for generation
  }
  deriving stock (Show, Eq, Generic)

-- | Temperature parameter for LLM generation (0.0 to 1.0).
--
-- Validated during construction and deserialization to ensure it falls within
-- the valid range.
newtype Temperature = Temperature Float
  deriving stock (Show, Generic)
  deriving newtype (Eq)

-- | Smart constructor for `Temperature` that validates the range.
mkTemperature :: Float -> Maybe Temperature
mkTemperature f
  | f >= 0.0 && f <= 1.0 = Just $ Temperature f
  | otherwise = Nothing
