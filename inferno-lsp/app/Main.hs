{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Inferno.Eval.Error (EvalError)
import Inferno.LSP.Server (runInfernoLspServer)
import Inferno.Module.Prelude (builtinModules)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import Control.Monad.Except (ExceptT)

main :: IO ()
main = do
  runInfernoLspServer @() @IO builtinModules >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c
