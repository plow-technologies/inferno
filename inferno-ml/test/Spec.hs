{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Data.Map as Map
import Data.Text (Text, unpack)
import Inferno.Core (InfernoError (..), Interpreter (..), mkInferno)
import Inferno.ML.Module.Prelude (mlPrelude)
import Inferno.ML.Types.Value (MlValue (VTensor))
import Inferno.Types.Value (Value (..))
import Inferno.Utils.Prettyprinter (renderPretty)
import Test.Hspec (Spec, describe, expectationFailure, hspec, it, runIO, shouldBe)
import qualified Torch as T
import qualified Torch.DType as TD

main :: IO ()
main =
  hspec $ do
    evalTests

xorScript :: Text
xorScript =
  "\
  \ open ML in                                                                         \n\
  \ // These are the parameters from a model trained offline with hasktorch + Haskell: \n\
  \ let w1 = asTensor2 #float [[-1.1163   ,  1.1485   ], [-1.5528   ,  1.6075   ]] in  \n\
  \ let b1 = asTensor1 #float [-0.5647   ,  0.8927   ] in                              \n\
  \ let w2 = asTensor2 #float [[ 1.2794   , -1.1922   ]] in                            \n\
  \ let b2 = asTensor1 #float [ 1.5040   ] in                                          \n\
  \ // This is an inferno function that runs inference using the trained model:        \n\
  \ let mlp = fun x ->                                                                 \n\
  \   let y1 = add (matmul x (transpose2D w1)) b1 in                                   \n\
  \   let y2 = add (matmul (tanH y1) (transpose2D w2)) b2 in                           \n\
  \   y2                                                                               \n\
  \ in                                                                                 \n\
  \ // Let's check that it computes XOR correctly on some inputs:                      \n\
  \ let inputs = Array.map (asTensor1 #float) [ [0, 0], [0, 1], [1, 0], [1, 1]]        \n\
  \ in                                                                                 \n\
  \ Array.map round (Array.map asDouble (Array.map mlp inputs))                        \n\
  \"

evalTests :: Spec
evalTests = describe "evaluate" $
  do
    Interpreter {evalExpr, defaultEnv, parseAndInferTypeReps} <-
      runIO $ (mkInferno mlPrelude :: IO (Interpreter MlValue))
    let shouldEvaluateInEnvTo implEnv str (v :: Value MlValue IO) =
          it ("\"" <> unpack str <> "\" should evaluate to " <> (unpack $ renderPretty v)) $ do
            case parseAndInferTypeReps str of
              Left err -> expectationFailure $ show err
              Right ast ->
                evalExpr defaultEnv implEnv ast >>= \case
                  Left err -> expectationFailure $ "Failed eval with: " <> show err
                  Right v' -> (renderPretty v') `shouldBe` (renderPretty v)
    let shouldEvaluateTo = shouldEvaluateInEnvTo Map.empty
    let shouldFailToInferTypeFor str =
          it ("should fail to infer type of \"" <> unpack str <> "\"") $
            case parseAndInfer str of
              Left (ParseError err) -> expectationFailure err
              Left (PinError _err) -> pure ()
              Left (InferenceError _err) -> pure ()
              Right _ -> expectationFailure $ "Should fail to infer a type"

    shouldEvaluateTo "open ML in asTensor1 #int [1, 2, 4]" $
      VCustom $
        VTensor $
          T.asTensor [1, 2, 4 :: Int]
    shouldEvaluateTo "open ML in asTensor2 #float [[1, 2, 4]]" $
      VCustom $
        VTensor $
          T.toType TD.Float $
            T.asTensor [[1, 2, 4 :: Double]]
    shouldFailToInferTypeFor "open ML in asTensor4 #float [[1, 2, 4]]"
    shouldEvaluateTo "open ML in asDouble (sumAll (ones #int [2, 4]))" $ VDouble 8.0
    shouldEvaluateTo xorScript $ VArray (map VInt [0, 1, 1, 0])
