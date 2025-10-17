{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | These are type definitions for per-server settings, i.e. settings we only
-- want applied to a single instance of an Inferno ML server. This is separate
-- from the general, universal settings for Inferno ML images
module Inferno.ML.Server.Types.PerServer where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value,
    object,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Plow.Logging.Message
  ( LogLevel (LevelDebug, LevelError, LevelInfo, LevelWarn),
  )
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

data PerServerConfig = PerServerConfig
  { instanceId :: Text
  -- ^ ID of the instance that @inferno-ml-server@ is running on. This is
  -- needed for tracing to the DB. In real-world cases this will be an EC2
  -- instance ID
  , logLevel :: LogLevel
  -- ^ Minimum log level for this instance
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PerServerConfig where
  parseJSON = withObject "PerServerConfig" $ \o ->
    PerServerConfig
      <$> o .: "instance-id"
      <*> (maybe (pure LevelInfo) decodeLogLevel =<< o .:? "log-level")

instance ToJSON PerServerConfig where
  toJSON cfg =
    object
      [ "instance-id" .= cfg.instanceId
      , "log-level" .= encodeLogLevel cfg.logLevel
      ]

-- | The JSON (de)serialization of 'LogLevel' appears to be broken, with what
-- seems to be a generically derived @ToJSON@ instance (encoded as an @Object@)
-- but a @FromJSON@ instance that expects an Aeson @String@. This provides a
-- working parser.
--
-- Example of broken 'LogLevel' serde:
--
-- >>> encode LevelInfo
-- "{\"tag\":\"LevelInfo\"}"
--
-- >>> eitherDecode @LogLevel it
-- Left "Error in $: parsing LogLevel failed, expected String, but encountered Object"
--
-- Whereas:
--
-- >>> encodeLogLevel LevelInfo
-- String "info"
--
-- >>> parseEither decodeLogLevel it
-- Right INFO
--
-- NOTE: For the sake of simplicity and round-tripping with 'encodeLogLevel',
-- @LevelOther@ is not supported
decodeLogLevel :: Value -> Parser LogLevel
decodeLogLevel = withText "LogLevel" $ \case
  "debug" -> pure LevelDebug
  "info" -> pure LevelInfo
  "warn" -> pure LevelWarn
  "error" -> pure LevelError
  t ->
    fail $
      unwords
        [ "Unrecognized log level:"
        , Text.unpack t
        , "(note: `LevelOther` not supported)"
        ]

-- | See 'decodeLogLevel'
encodeLogLevel :: LogLevel -> Value
encodeLogLevel = toJSON . fmap toLower . show
