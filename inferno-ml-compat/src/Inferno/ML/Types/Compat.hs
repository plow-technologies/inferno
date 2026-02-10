{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors #-}

module Inferno.ML.Types.Compat where

import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

-- | Sum type representing different model configuration types. The type
-- parameter represents the contents of a TorchScript model; for DB storage
-- this is an @Oid@, while for runtime evaluation this would be a @ScriptModule@
data ModelConfig a
  = TorchScript a
  | Bedrock BedrockConfig
  deriving stock (Show, Eq, Generic, Functor)

-- | Configuration for Bedrock-based models
data BedrockConfig = BedrockConfig
  { modelId :: Text
  -- ^ The *Bedrock* model identifier (e.g., "anthropic.claude-3-5-sonnet-20241022-v2:0")
  , temperature :: Temperature
  -- ^ Controls randomness in token selection; lower values produce more
  -- deterministic responses, higher values produce more random/creative ones
  , topP :: TopP
  -- ^ Nucleus sampling parameter; considers only tokens comprising the top
  -- cumulative probability mass (e.g. @0.9@ means only tokens in the top 90%
  -- probability are considered)
  , stopSequences :: StopSequences
  -- ^ Character sequences that halt token generation when encountered.
  -- Capped at 4 elements, each at most 50 characters. In practice, most models
  -- do not handle stop sequences well; leave this empty unless you have a /very/
  -- specific use case. Do NOT use it for ad-hoc pattern-matching, regexes, etc...
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

-- | Controls randomness in token selection during LLM inference. Lower values
-- (e.g. @0.2@) produce more deterministic, focused responses; higher values
-- (e.g. @0.8@) produce more random, creative responses. Must be in range
-- @0.0@ to @1.0@.
--
-- Validated during construction and deserialization to ensure it falls within
-- the valid range.
newtype Temperature = Temperature Normalized
  deriving stock (Show, Generic)
  deriving newtype (Eq)

-- | Smart constructor for 'Temperature' that validates the range.
mkTemperature :: Float -> Maybe Temperature
mkTemperature = fmap Temperature . mkNormalized

-- | Nucleus sampling parameter for LLM inference. Limits token selection to
-- only those comprising the top cumulative probability mass. For example,
-- @0.9@ means only tokens in the top 90% of the probability distribution are
-- considered. Lower values produce more focused outputs; higher values allow
-- more diversity. Must be in range @0.0@ to @1.0@
--
-- Validated during construction and deserialization to ensure it falls within
-- the valid range.
newtype TopP = TopP Normalized
  deriving stock (Show, Generic)
  deriving newtype (Eq)

-- | Smart constructor for 'TopP' that validates the range.
mkTopP :: Float -> Maybe TopP
mkTopP = fmap TopP . mkNormalized

-- | Character sequences that halt token generation when encountered during
-- LLM inference. Useful for marking clear boundaries in generated text IF
-- it is essential to do so
--
-- Capped at 4 elements during parsing, with each element limited to 50
-- characters. Note that while AWS Bedrock allows up to 2,500 stop sequences,
-- in practice most models do not handle more than a few, and using stop
-- sequences at all can lead to abrupt truncations. Leave this empty unless
-- you have a /very/ specific use case
newtype StopSequences = StopSequences (Vector Text)
  deriving stock (Show, Generic)
  deriving newtype (Eq)
