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
    Object,
    ToJSON (toJSON),
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Plow.Logging.Message (LogLevel (LevelError, LevelInfo, LevelWarn))
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
  -- needed for tracing to the DB
  , logLevel :: LogLevel
  -- ^ Minimum log level for this instance
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PerServerConfig where
  parseJSON = withObject "PerServerConfig" $ \o ->
    PerServerConfig
      <$> o .: "instance-id"
      <*> fmap (fromMaybe LevelInfo) (logLevelP o)
    where
      logLevelP :: Object -> Parser (Maybe LogLevel)
      logLevelP o =
        o .:? "log-level" >>= \case
          Just i@"info" -> pure $ toLogLevel i
          Just i@"warn" -> pure $ toLogLevel i
          Just i@"error" -> pure $ toLogLevel i
          _ -> pure Nothing

      toLogLevel :: Text -> Maybe LogLevel
      toLogLevel = \case
        "info" -> pure LevelInfo
        "warn" -> pure LevelWarn
        "error" -> pure LevelError
        _ -> Nothing

instance ToJSON PerServerConfig where
  toJSON cfg =
    object
      [ "instance-id" .= cfg.instanceId
      , "log-level" .= toJSON (fmap toLower (show cfg.logLevel))
      ]
