{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.Text.IO as Text
import Inferno.Core (Interpreter (parseAndEval), mkInferno)
import Inferno.Module.Prelude (builtinModules)
import Inferno.Utils.Prettyprinter (showPretty)
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  src <- Text.readFile file
  let inferno = mkInferno builtinModules :: Interpreter ()
  parseAndEval inferno src >>= \case
    Left err -> print err
    Right res -> showPretty res