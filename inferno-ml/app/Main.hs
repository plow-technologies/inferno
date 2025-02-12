{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import Inferno.Core (Interpreter (..), mkInferno)
import Inferno.ML.Module.Prelude (mlPrelude)
import Inferno.ML.Types.Value (MlValue, customTypes)
import Inferno.Utils.Prettyprinter (showPretty)
import System.Environment (getArgs)

main :: IO ()
main = do
  getArgs >>= \case
    file : _ -> do
      src <- Text.readFile file
      Interpreter {evalExpr, defaultEnv, parseAndInferTypeReps} <-
        mkInferno @_ @(MlValue ()) mlPrelude customTypes
      case parseAndInferTypeReps src of
        Left err -> print err
        Right ast ->
          evalExpr defaultEnv Map.empty ast >>= \case
            Left err -> print err
            Right res -> showPretty res
    _ -> error "Usage: inferno-ml-exe FILE"
