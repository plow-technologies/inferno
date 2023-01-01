{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Inferno.Eval.Error (EvalError)
import Inferno.LSP.Server (runInfernoLspServer)
import Inferno.Module.Prelude (builtinModules)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)

main :: IO ()
main = do
  runInfernoLspServer @() @(Either EvalError) builtinModules >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c
