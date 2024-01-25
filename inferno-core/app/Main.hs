{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import Inferno.Core (Interpreter (..), mkInferno)
import Inferno.Module.Prelude (ModuleMap, baseOpsTable, builtinModules, builtinModulesOpsTable)
import Inferno.Parse (parseExpr)
import Inferno.Utils.Prettyprinter (showPretty)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  file <- head <$> getArgs
  src <- Text.readFile file
  Interpreter {evalExpr, defaultEnv, parseAndInferTypeReps} <-
    mkInferno builtinModules [] :: IO (Interpreter IO ())
  let prelude :: ModuleMap IO () = builtinModules
  case parseExpr (baseOpsTable prelude) (builtinModulesOpsTable prelude) [] src of
    Left err ->
      hPutStrLn stderr $ show err
    Right (ast, comments) ->
      showPretty ast

-- case parseAndInferTypeReps src of
--   Left err -> do
--     hPutStrLn stderr $ show err
--     exitFailure
--   Right ast ->
--     evalExpr defaultEnv Map.empty ast >>= \case
--       Left err -> do
--         hPutStrLn stderr $ show err
--         exitFailure
--       Right res -> showPretty res
