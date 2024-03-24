{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Bifunctor (bimap)
import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import Inferno.Core (Interpreter (..), mkInferno)
import Inferno.Module.Prelude (builtinModules)
import Inferno.Types.VersionControl (pinnedToMaybe)
import Inferno.Utils.Prettyprinter (showPretty)
import Options.Applicative
  ( Parser,
    argument,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    progDesc,
    short,
    str,
    switch,
    (<**>),
  )
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)

data CliArgs = CliArgs {file :: String, typecheck :: Bool}

cliargs :: Parser CliArgs
cliargs =
  CliArgs
    <$> argument str (metavar "FILE" <> help "Input file path")
    <*> switch (long "typecheck" <> short 't' <> help "Only run type inference")

main :: IO ()
main = do
  let opts =
        info
          (cliargs <**> helper)
          ( fullDesc
              <> progDesc "Run Inferno on FILE"
              <> header "inferno - a functional scripting language"
          )
  args <- execParser opts

  src <- Text.readFile $ file args
  Interpreter {evalExpr, defaultEnv, parseAndInfer} <-
    mkInferno builtinModules [] :: IO (Interpreter IO ())
  case parseAndInfer src of
    Left err -> do
      hPrint stderr err
      exitFailure
    Right (ast, ty, _, _) -> do
      if typecheck args
        then do
          putStrLn "Inferred type:"
          showPretty ty
        else do
          let ast' = bimap pinnedToMaybe (const ()) ast
          evalExpr defaultEnv Map.empty ast' >>= \case
            Left err -> do
              hPrint stderr err
              exitFailure
            Right res -> showPretty res
