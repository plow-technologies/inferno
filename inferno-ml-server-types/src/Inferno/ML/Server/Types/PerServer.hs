{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | These are type definitions for per-server settings, i.e. settings we only
-- want applied to a single instance of an Inferno ML server. This is separate
-- from the general, universal settings for Inferno ML images
module Inferno.ML.Server.Types.PerServer where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    camelTo2,
    defaultOptions,
    fieldLabelModifier,
    genericParseJSON,
    genericToJSON,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
  ( Get,
    JSON,
    Post,
    ReqBody,
    (:<|>),
    (:>),
  )

-- | This API is /not/ meant to be implemented by @inferno-ml-server@ itself,
-- but by another service that writes a configuration file that @inferno-ml-server@
-- requires at startup
type PerServerAPI =
  "inferno-ml" :> "configure" :> "set" :> ReqBody '[JSON] PerServerConfig :> Post '[JSON] ()
    :<|> "inferno-ml" :> "configure" :> "get" :> Get '[JSON] PerServerConfig

newtype PerServerConfig = PerServerConfig
  { instanceId :: Text
  -- ^ ID of the instance that @inferno-ml-server@ is running on. This is
  -- needed for tracing to the DB
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PerServerConfig where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '-'
        }

instance ToJSON PerServerConfig where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '-'
        }
