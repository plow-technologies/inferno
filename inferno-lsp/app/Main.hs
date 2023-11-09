{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Inferno.LSP.Server (runInfernoLspServer)
import Inferno.Module.Prelude (builtinPrelude)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)

main :: IO ()
main = do
  runInfernoLspServer @() builtinPrelude [] >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c
