{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Bifunctor (bimap, second)
import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import Inferno.Eval (runEvalM)
import Inferno.Infer (inferExpr)
import Inferno.Infer.Pinned (pinExpr)
import Inferno.Module.Prelude (baseOpsTable, builtinModules, builtinModulesOpsTable, builtinModulesPinMap, builtinModulesTerms)
import Inferno.Parse (parseExpr)
import Inferno.Parse.Commented (insertCommentsIntoExpr)
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

data CliArgs = CliArgs {file :: String, typecheck :: Bool, parse :: Bool}

cliargs :: Parser CliArgs
cliargs =
  CliArgs
    <$> argument str (metavar "FILE" <> help "Input file path")
    <*> switch (long "typecheck" <> short 't' <> help "Only run type inference")
    <*> switch (long "parse" <> short 'p' <> help "Only run parser")

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
  let customTypes = []
  let prelude = builtinModules @IO @()
  let defaultEnv = builtinModulesTerms prelude
  -- parse
  case parseExpr (baseOpsTable prelude) (builtinModulesOpsTable prelude) customTypes src of
    Left err -> do
      hPrint stderr err
      exitFailure
    Right (ast, _comments) ->
      if parse args
        then do
          let ast' = insertCommentsIntoExpr _comments ast
          putStrLn "Parsed Expr:"
          print $ second (const ()) ast'
          putStrLn "Pretty:"
          showPretty ast'
        else do
          -- pin free variables to builtin prelude function hashes
          case pinExpr (builtinModulesPinMap prelude) ast of
            Left err -> do
              hPrint stderr err
              exitFailure
            Right pinnedAST ->
              -- typecheck
              case inferExpr prelude pinnedAST of
                Left err -> do
                  hPrint stderr err
                  exitFailure
                Right (ast', ty, _tyMap) ->
                  if typecheck args
                    then do
                      putStrLn "Inferred type:"
                      showPretty ty
                    else do
                      let ast'' = bimap pinnedToMaybe (const ()) ast'
                      runEvalM defaultEnv Map.empty ast'' >>= \case
                        Left err -> do
                          hPrint stderr err
                          exitFailure
                        Right res -> showPretty res
