module Inferno.ML.Remote.Types
  ( InfernoMlRemoteAPI,
    Options (..),
    Script (..),
    EvalResult (..),
    InfernoMlRemoteM,
    InfernoMlRemoteEnv (..),
    ModelLoader (..),
    parseOptions,
  )
where

import Control.Applicative ((<**>))
import Control.Monad.Reader (ReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import qualified Options.Applicative as Options
import Servant (Handler, JSON, Post, ReqBody, Server, (:>))

-- TODO
-- Use more descriptive types. Implied `Text -> Text` is pretty awful
type InfernoMlRemoteAPI =
  "inference" :> ReqBody '[JSON] Script :> Post '[JSON] EvalResult

type InfernoMlRemoteM = ReaderT InfernoMlRemoteEnv Handler

newtype InfernoMlRemoteEnv = InfernoMlRemoteEnv
  { loader :: ModelLoader
  }
  deriving stock (Generic)

-- TODO
-- Add more ways to load?
data ModelLoader
  = -- | Path to directory holding models
    Paths FilePath
  | -- | Path to directory holding compressed models
    CompressedPaths FilePath
  deriving stock (Show, Eq, Generic)

newtype Options = Options
  { port :: Word64
  }
  deriving stock (Show, Eq, Generic)

newtype Script = Script Text
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON)

newtype EvalResult = EvalResult Text
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON)

parseOptions :: IO Options
parseOptions = Options.execParser opts
  where
    opts :: Options.ParserInfo Options
    opts =
      Options.info (optionsP <**> Options.helper) $
        Options.fullDesc <> Options.progDesc "Server for `inferno-ml-remote`"

    optionsP :: Options.Parser Options
    optionsP =
      Options
        <$> Options.option
          Options.auto
          ( Options.long "port"
              <> Options.short 'p'
              <> Options.help "Port to run `inferno-ml-remote` server on"
              <> Options.showDefault
              <> Options.value 8080
              <> Options.metavar "UINT"
          )
