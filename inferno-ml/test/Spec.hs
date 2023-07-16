{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Data.Map as Map
import Data.Text (unpack)
import Inferno.Core (InfernoError (..), Interpreter (evalInEnv, parseAndInfer, parseAndInferTypeReps), mkInferno)
import Inferno.ML.Module.Prelude (mlPrelude)
import Inferno.ML.Types.Value (MlValue (VTensor))
import Inferno.Types.Value (Value (..))
import Inferno.Utils.Prettyprinter (renderPretty)
import Test.Hspec (Spec, describe, expectationFailure, hspec, it, shouldBe)
import qualified Torch as T
import qualified Torch.DType as TD

main :: IO ()
main =
  hspec $ do
    evalTests

evalTests :: Spec
evalTests = describe "evaluate" $
  do
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
  where
    inferno = mkInferno mlPrelude :: Interpreter MlValue
    shouldEvaluateInEnvTo localEnv implEnv str (v :: Value MlValue IO) =
      it ("\"" <> unpack str <> "\" should evaluate to " <> (unpack $ renderPretty v)) $ do
        case parseAndInferTypeReps inferno str of
          Left err -> expectationFailure $ show err
          Right ast ->
            evalInEnv inferno localEnv implEnv ast >>= \case
              Left err -> expectationFailure $ "Failed eval with: " <> show err
              Right v' -> (renderPretty v') `shouldBe` (renderPretty v)
    shouldEvaluateTo = shouldEvaluateInEnvTo Map.empty Map.empty
    _shouldThrowRuntimeError str merr =
      it ("\"" <> unpack str <> "\" should throw a runtime error") $ do
        case parseAndInferTypeReps inferno str of
          Left err -> expectationFailure $ show err
          Right ast ->
            evalInEnv inferno Map.empty Map.empty ast >>= \case
              Left err' -> case merr of
                Nothing -> pure ()
                Just err -> err' `shouldBe` err
              Right _ -> expectationFailure $ "Should not evaluate."
    shouldFailToInferTypeFor str =
      it ("should fail to infer type of \"" <> unpack str <> "\"") $
        case parseAndInfer inferno str of
          Left (ParseError err) -> expectationFailure err
          Left (PinError _err) -> pure ()
          Left (InferenceError _err) -> pure ()
          Right _ -> expectationFailure $ "Should fail to infer a type"
