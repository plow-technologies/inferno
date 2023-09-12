{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.Map as M
import qualified Data.Text.IO as Text
import Inferno.Core (Interpreter (evalInEnv, parseAndInferTypeReps), mkInferno)
import Inferno.Module.Prelude (builtinModules)
import Inferno.Utils.Prettyprinter (showPretty)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  file <- head <$> getArgs
  src <- Text.readFile file
  let inferno = mkInferno builtinModules :: Interpreter ()
  case parseAndInferTypeReps inferno src of
    Left err -> do
      hPutStrLn stderr $ show err
      exitFailure
    Right ast ->
      evalInEnv inferno M.empty M.empty ast >>= \case
        Left err -> do
          hPutStrLn stderr $ show err
          exitFailure
        Right res -> showPretty res
