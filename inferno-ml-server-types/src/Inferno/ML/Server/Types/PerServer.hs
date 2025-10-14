{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | These are type definitions for per-server settings, i.e. settings we only
-- want applied to a single instance of an Inferno ML server. This is separate
-- from the general, universal settings for Inferno ML images
module Inferno.ML.Server.Types.PerServer where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

newtype PerServerConfig = PerServerConfig
  { instanceId :: Text
    -- ^ ID of the instance that @inferno-ml-server@ is running on. This is
    -- needed for tracing to the DB
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
