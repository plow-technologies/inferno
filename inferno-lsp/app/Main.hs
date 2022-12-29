{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Inferno.LSP.Server (runInfernoLspServer)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import Inferno.Module.Prelude (builtinModules)
import Inferno.Eval.Error (EvalError)


main :: IO ()
main = do
  runInfernoLspServer @() @(Either EvalError) builtinModules >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c
