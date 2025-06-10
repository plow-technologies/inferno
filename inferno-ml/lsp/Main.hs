{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Inferno.LSP.Server (runInfernoLspServer)
import Inferno.ML.Module.Prelude (defaultMlPrelude)
import Inferno.ML.Types.Value (MlValue, customTypes)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)

main :: IO ()
main = do
  runInfernoLspServer @(MlValue ()) defaultMlPrelude customTypes >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c
