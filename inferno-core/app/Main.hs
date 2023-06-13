{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.Map as M
import qualified Data.Text.IO as Text
import Inferno.Core (Interpreter (evalInEnv, parseAndInfer), mkInferno)
import Inferno.Module.Prelude (builtinModules)
import Inferno.Utils.Prettyprinter (showPretty)
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  src <- Text.readFile file
  let inferno = mkInferno builtinModules :: Interpreter ()
  case parseAndInfer inferno src of
    Left err -> print err
    Right ast ->
      evalInEnv inferno M.empty M.empty ast >>= \case
        Left err -> print err
        Right res -> showPretty res