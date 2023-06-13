{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.Map as M
import qualified Data.Text.IO as Text
import Inferno.Core (Interpreter (evalInEnv, parseAndInfer), mkInferno)
import Inferno.ML.Module.Prelude (mlPrelude)
import Inferno.ML.Types.Value (MlValue)
import Inferno.Utils.Prettyprinter (showPretty)
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  src <- Text.readFile file
  let inferno = mkInferno mlPrelude :: Interpreter MlValue
  case parseAndInfer inferno src of
    Left err -> print err
    Right ast ->
      evalInEnv inferno M.empty M.empty ast >>= \case
        Left err -> print err
        Right res -> showPretty res