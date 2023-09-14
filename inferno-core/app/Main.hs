{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import Inferno.Core (Interpreter (evalExpr, parseAndInferTypeReps), defaultEnv, mkInferno)
import Inferno.Module.Prelude (builtinModules)
import Inferno.Utils.Prettyprinter (showPretty)
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  src <- Text.readFile file
  inferno <- mkInferno builtinModules :: IO (Interpreter ())
  case parseAndInferTypeReps inferno src of
    Left err -> print err
    Right ast ->
      evalExpr inferno (defaultEnv inferno) Map.empty ast >>= \case
        Left err -> print err
        Right res -> showPretty res
