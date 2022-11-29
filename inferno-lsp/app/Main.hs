{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

-- import Inferno.LSP.Server (runInfernoLspServer)
-- import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)

main :: IO ()
main = putStrLn "NOTE: the commented code in this file demonstrates how to instantiate Inferno with custom value types and obtain an executable."

-- main = do
-- runInfernoLspServer @ValueType preludeNameToTypeMap >>= \case
--   0 -> exitSuccess
--   c -> exitWith . ExitFailure $ c
