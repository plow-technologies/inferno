module Main (main) where

import Eval.Spec (evalTests)
import Golden.Spec (goldenTests)
import Infer.Spec (inferTests)
import Parse.Spec (parsingTests)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Test.Framework (htfMain, makeTestSuite)
import Test.Framework.BlackBoxTest (blackBoxTests, defaultBBTArgs)
import Test.Hspec (hspec)

main :: IO ()
main = do
  lookupEnv "INFERNO_EXE" >>= \case
    Nothing -> do
      putStrLn "Error: environment variable INFERNO_EXE undefined. Set to path of the inferno executable"
      exitFailure
    Just infernoExePath -> do
      hspec $ do
        parsingTests
        inferTests
        evalTests
        goldenTests

      -- We run the HTF black box (or end-to-end) tests at the end because htfMain
      -- exits the program even upon sucess, so anything after this is not run.
      bbts <- blackBoxTests "." infernoExePath ".inferno" defaultBBTArgs
      htfMain [makeTestSuite "bbts" bbts]
