module Main (main) where

import Eval.Spec (evalTests)
import Golden.Spec (goldenTests)
import Infer.Spec (inferTests)
import Parse.Spec (parsingTests)
import Test.Hspec (hspec)
import Test.Framework ( htfMain, makeTestSuite )
import Test.Framework.BlackBoxTest ( blackBoxTests, defaultBBTArgs )

main :: IO ()
main = do
  bbts <- blackBoxTests "." "/home/sid/inferno/dist-newstyle/build/x86_64-linux/ghc-9.2.5/inferno-core-0.4.0.0/x/inferno/build/inferno/inferno" ".inferno" defaultBBTArgs
  htfMain [makeTestSuite "bbts" bbts]

  hspec $ do
    parsingTests
    inferTests
    evalTests
    goldenTests
