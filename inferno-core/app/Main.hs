{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import Inferno.Core (Interpreter (..), mkInferno)
import Inferno.Module.Prelude (builtinModules)
import Inferno.Utils.Prettyprinter (showPretty)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  file <- head <$> getArgs
  src <- Text.readFile file
  Interpreter {evalExpr, defaultEnv, parseAndInferTypeReps, parseAndInfer} <-
    mkInferno builtinModules [] :: IO (Interpreter IO ())
  case parseAndInfer src of
    Left err -> do
      hPutStrLn stderr $ show err
      exitFailure
    Right (ast, ty, _, _) -> do
      putStrLn "Inferred type:"
      showPretty ty

-- case parseAndInferTypeReps src of
--   Left err -> do
--     hPutStrLn stderr $ show err
--     exitFailure
--   Right ast -> do
--     evalExpr defaultEnv Map.empty ast >>= \case
--       Left err -> do
--         hPutStrLn stderr $ show err
--         exitFailure
--       Right res -> showPretty res
