{-# LANGUAGE StandaloneDeriving #-}

module Inferno.ML.Remote.Types
  ( InfernoMlRemoteAPI,
    Options (..),
    Script (..),
    EvalResult (..),
    InfernoMlRemoteM,
    InfernoMlRemoteEnv (..),
    ModelCacheOption (..),
    ModelCache (..),
    SomeInfernoError (..),
    InfernoMlRemoteError (..),
    parseOptions,
    mkOptions,
  )
where

import Control.Applicative (Alternative ((<|>)), optional, (<**>))
import Control.Exception (Exception (displayException), throwIO)
import Control.Monad.Reader (ReaderT)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON,
    eitherDecodeFileStrict,
    withObject,
    (.:),
    (.:?),
  )
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import GHC.Generics (Generic)
import qualified Options.Applicative as Options
import Servant (Handler, JSON, Post, ReqBody, (:>))

type InfernoMlRemoteAPI =
  "inference" :> ReqBody '[JSON] Script :> Post '[JSON] EvalResult

type InfernoMlRemoteM = ReaderT InfernoMlRemoteEnv Handler

newtype InfernoMlRemoteEnv = InfernoMlRemoteEnv
  { modelCache :: Maybe ModelCacheOption
  }
  deriving stock (Generic)

-- | Generic container for errors that may arise when parsing\/typechecking
-- Inferno scripts in handlers. It doesn\'t matter what the specific error is
-- as it will only be used in a 400 or 500 response with a message body containing
-- the error
data SomeInfernoError where
  SomeInfernoError :: forall a. Show a => a -> SomeInfernoError

deriving stock instance Show SomeInfernoError

instance Exception SomeInfernoError where
  displayException (SomeInfernoError x) = show x

-- TODO
-- Add more ways to load?
data ModelCacheOption
  = -- | Path to source directory holding models
    Paths FilePath ModelCache
  | -- | Path to source directory holding compressed models
    CompressedPaths FilePath ModelCache
  deriving stock (Show, Eq, Generic)

instance FromJSON ModelCacheOption where
  parseJSON = withObject "ModelCacheOption" $ \o ->
    o .:? "compressed" >>= \case
      Just True -> CompressedPaths <$> o .: "store" <*> o .: "cache"
      _ -> Paths <$> o .: "store" <*> o .: "cache"

-- | Options for caching ML models to be used with Inferno scripts. When a script
-- uses @ML.loadModel@, models will be copied from the source configured in
-- 'ModelCacheOption' and saved to the 'cache' directory. Once the `maxSize` has
-- been exceeded, least-recently-used cached models will be removed
data ModelCache = ModelCache
  { -- | Directry where the models should be cached
    path :: FilePath,
    -- | Maximum size in bytes of the model cache directory
    maxSize :: Word64
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ModelCache where
  parseJSON = withObject "ModelCache" $ \o ->
    ModelCache <$> o .: "path" <*> o .: "max-size"

data Options = Options
  { port :: Word64,
    modelCache :: Maybe ModelCacheOption
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Options where
  parseJSON = withObject "Options" $ \o ->
    Options <$> o .: "port" <*> o .:? "model-cache"

newtype Script = Script Text
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON, IsString)

newtype EvalResult = EvalResult Text
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON, IsString)

data InfernoMlRemoteError
  = CacheSizeExceeded
  | NoSuchModel Text
  | ExternalProcessFailed FilePath Int
  deriving stock (Show, Eq, Generic)

instance Exception InfernoMlRemoteError where
  displayException = \case
    CacheSizeExceeded -> "Model exceeds maximum cache size"
    ExternalProcessFailed p ec ->
      unwords
        [ "Process",
          "'" <> p <> "'",
          "failed with exit code",
          show ec
        ]
    NoSuchModel m ->
      unwords
        [ "Model:",
          "'" <> Text.unpack m <> "'",
          "does not exist in the store"
        ]

mkOptions :: IO Options
mkOptions =
  parseOptions >>= \case
    CliOptions os -> pure os
    ConfigFile fp ->
      eitherDecodeFileStrict @Options fp
        >>= either (throwIO . userError) pure

data OptionsOrCfgFile
  = CliOptions Options
  | ConfigFile FilePath
  deriving stock (Show, Eq, Generic)

parseOptions :: IO OptionsOrCfgFile
parseOptions = Options.execParser opts
  where
    opts :: Options.ParserInfo OptionsOrCfgFile
    opts =
      Options.info ((cfgFileP <|> optionsP) <**> Options.helper) $
        Options.fullDesc <> Options.progDesc "Server for `inferno-ml-remote`"

    cfgFileP :: Options.Parser OptionsOrCfgFile
    cfgFileP =
      ConfigFile
        <$> Options.strOption
          ( Options.long "config" <> Options.metavar "FILEPATH"
          )

    optionsP :: Options.Parser OptionsOrCfgFile
    optionsP =
      fmap CliOptions $
        Options
          <$> Options.option
            Options.auto
            ( Options.long "port"
                <> Options.short 'p'
                <> Options.help "Port to run `inferno-ml-remote` server on"
                <> Options.showDefault
                <> Options.value 8080
                <> Options.metavar "BYTES"
            )
          <*> modelCacheOptionP

    modelCacheOptionP :: Options.Parser (Maybe ModelCacheOption)
    modelCacheOptionP = optional $ pathsP <|> compressedPathsP
      where
        compressedPathsP :: Options.Parser ModelCacheOption
        compressedPathsP =
          pathP CompressedPaths
            <* Options.flag'
              ()
              ( Options.long "compressed"
                  <> Options.help "Whether the model source uses compression"
              )

        pathsP :: Options.Parser ModelCacheOption
        pathsP = pathP Paths

        pathP :: (FilePath -> ModelCache -> b) -> Options.Parser b
        pathP f =
          f
            <$> Options.strOption
              ( Options.long "model-source" <> Options.metavar "FILEPATH"
              )
            <*> modelCacheP

        modelCacheP :: Options.Parser ModelCache
        modelCacheP =
          ModelCache
            <$> Options.strOption
              ( Options.long "cache-path" <> Options.metavar "FILEPATH"
              )
            <*> Options.option
              Options.auto
              ( Options.long "max-size"
                  <> Options.help "Max size of model cache"
                  <> Options.metavar "BYTES"
              )
