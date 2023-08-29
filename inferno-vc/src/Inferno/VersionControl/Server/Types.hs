{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Inferno.VersionControl.Server.Types where

import Data.Aeson.Types
import Data.ByteString (readFile)
import Data.Text (Text)
import Data.Yaml
import GHC.Generics (Generic)

data ServerConfig = ServerConfig
  { serverHost :: Text,
    serverPort :: Int,
    vcPath :: FilePath
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ServerConfig where
  parseJSON = withObject "ServerConfig" $ \o -> do
    server <- o .: "server"
    serverHost <- server .: "host"
    serverPort <- server .: "port"
    vcPath <- server .: "vcPath"
    pure ServerConfig {serverHost, serverPort, vcPath}

readServerConfig ::
  FromJSON config =>
  FilePath ->
  IO (Either String config)
readServerConfig fp = do
  f <- Data.ByteString.readFile fp
  return $ either (Left . prettyPrintParseException) Right $ decodeEither' f
