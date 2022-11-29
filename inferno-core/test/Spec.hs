module Main (main) where

import Eval.Spec (evalTests)
import Golden.Spec (goldenTests)
import Infer.Spec (inferTests)
import Parse.Spec (parsingTests)
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    parsingTests
    inferTests
    evalTests
    goldenTests
