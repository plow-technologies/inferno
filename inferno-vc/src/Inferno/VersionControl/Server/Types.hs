{-# LANGUAGE OverloadedStrings #-}

module Inferno.VersionControl.Server.Types where

import Data.Aeson.Types
import Data.ByteString (readFile)
import Data.Text (Text)
import Data.Yaml

data ServerConfig = ServerConfig
  { _serverHost :: Text,
    _serverPort :: Int,
    _vcPath :: FilePath
  }
  deriving (Show, Eq, Ord)

instance FromJSON ServerConfig where
  parseJSON = withObject "ServerConfig" $ \o -> do
    server <- o .: "server"
    serverHost <- server .: "host"
    serverPort <- server .: "port"
    vcPath <- server .: "vcPath"

    return $ ServerConfig serverHost serverPort vcPath

readServerConfig ::
  FilePath ->
  IO (Either String ServerConfig)
readServerConfig fp = do
  f <- Data.ByteString.readFile fp
  return $ either (Left . prettyPrintParseException) Right $ decodeEither' f
