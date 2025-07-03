{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Data.List.NonEmpty as NEList
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import Inferno.Core (InfernoError (..), Interpreter (..), mkInferno)
import Inferno.ML.Module.Prelude (defaultMlPrelude)
import Inferno.ML.Types.Value (MlValue, customTypes, pattern VTensor)
import Inferno.Parse.Error (prettyError)
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
  \ // These are the parameters from a model trained offline with hasktorch + Haskell:       \n\
  \ let w1 = ML.asTensor2 ML.#float [[-1.1163   ,  1.1485   ], [-1.5528   ,  1.6075   ]] in  \n\
  \ let b1 = ML.asTensor1 ML.#float [-0.5647   ,  0.8927   ] in                              \n\
  \ let w2 = ML.asTensor2 ML.#float [[ 1.2794   , -1.1922   ]] in                            \n\
  \ let b2 = ML.asTensor1 ML.#float [ 1.5040   ] in                                          \n\
  \ // This is an inferno function that runs inference using the trained model:              \n\
  \ let mlp = fun x ->                                                                       \n\
  \   let y1 = Tensor.add (Tensor.matmul x (Tensor.transpose2D w1)) b1 in                    \n\
  \   let y2 = Tensor.add (Tensor.matmul (Tensor.tanh y1) (Tensor.transpose2D w2)) b2 in     \n\
  \   y2                                                                                     \n\
  \ in                                                                                       \n\
  \ // Let's check that it computes XOR correctly on some inputs:                            \n\
  \ let inputs = Array.map (ML.asTensor1 ML.#float) [ [0, 0], [0, 1], [1, 0], [1, 1]]        \n\
  \ in                                                                                       \n\
  \ Array.map round (Array.map ML.asDouble (Array.map mlp inputs))                           \n\
  \"

evalTests :: Spec
evalTests = describe "evaluate" $
  do
    Interpreter{evalExpr, defaultEnv, parseAndInfer, parseAndInferTypeReps} <-
      runIO $ mkInferno @_ @(MlValue ()) defaultMlPrelude customTypes
    let shouldEvaluateInEnvTo implEnv str (v :: Value (MlValue ()) IO) =
          it ("\"" <> unpack str <> "\" should evaluate to " <> unpack (renderPretty v)) $ do
            case parseAndInferTypeReps str of
              Left err -> expectationFailure $ show err
              Right ast ->
                evalExpr defaultEnv implEnv ast >>= \case
                  Left err -> expectationFailure $ "Failed eval with: " <> show err
                  Right v' -> renderPretty v' `shouldBe` renderPretty v
    let shouldEvaluateTo = shouldEvaluateInEnvTo Map.empty
    let shouldFailToInferTypeFor str =
          it ("should fail to infer type of \"" <> unpack str <> "\"") $
            case parseAndInfer str of
              Left (ParseError err) -> expectationFailure $ prettyError $ fst $ NEList.head err
              Left (PinError _err) -> pure ()
              Left (InferenceError _err) -> pure ()
              Right _ -> expectationFailure "Should fail to infer a type"

    shouldEvaluateTo "ML.asTensor1 ML.#int [1, 2, 4]" $
      VCustom $
        VTensor $
          T.asTensor [1, 2, 4 :: Int]
    shouldEvaluateTo "ML.asTensor2 ML.#float [[1, 2, 4]]" $
      VCustom $
        VTensor $
          T.toType TD.Float $
            T.asTensor [[1, 2, 4 :: Double]]
    shouldEvaluateTo "ML.toType ML.#float (ML.asTensor1 ML.#int [1, 2, 4])" $
      VCustom . VTensor . T.toType TD.Float $
        T.asTensor [1, 2, 4 :: Double]
    shouldFailToInferTypeFor "ML.asTensor4 ML.#float [[1, 2, 4]]"
    shouldEvaluateTo "ML.asDouble (Tensor.sumAll (ML.ones ML.#int [2, 4]))" $ VDouble 8.0
    shouldEvaluateTo xorScript $ VArray (map VInt [0, 1, 1, 0])
