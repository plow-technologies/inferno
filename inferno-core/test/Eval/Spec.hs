{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Eval.Spec where

import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Bifunctor (bimap)
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Text (unpack)
import Inferno.Core (Interpreter (..), mkInferno)
import Inferno.Eval.Error (EvalError (..))
import Inferno.Module (Prelude (..))
import Inferno.Module.Builtin (enumBoolHash)
import Inferno.Module.Prelude (builtinPrelude)
import Inferno.Types.Syntax (BaseType (..), Expr (..), ExtIdent (..), Ident (..), InfernoType (..))
import Inferno.Types.Value (ImplEnvM (..), Value (..), liftImplEnvM)
import Inferno.Types.VersionControl (pinnedToMaybe)
import Inferno.Utils.Prettyprinter (renderPretty)
import Inferno.Utils.QQ.Module (builtinPreludeQuoter)
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)

type TestCustomValue = ()

runtimeTypeRepsTests :: Interpreter IO TestCustomValue -> Spec
runtimeTypeRepsTests Interpreter {evalExpr, defaultEnv, parseAndInfer} = describe "runtime type reps" $ do
  let expr_3 =
        case parseAndInfer "3" of
          Left err ->
            error $ "parseAndInfer failed with: " <> show err
          Right (expr', _typ, _typMap, _comments) ->
            pure $ bimap pinnedToMaybe (const ()) expr'

  it "test int type rep" $ do
    expr <- expr_3
    let expr_3_int = App expr (TypeRep () (TBase TInt))
    shouldEvaluateTo expr_3_int (VInt 3)

  it "test double type rep" $ do
    expr <- expr_3
    let expr_3_double = App expr (TypeRep () (TBase TDouble))
    shouldEvaluateTo expr_3_double (VDouble 3)
  where
    shouldEvaluateTo expr (v :: Value TestCustomValue IO) = do
      evalExpr defaultEnv Map.empty expr >>= \case
        Left err -> expectationFailure $ "Failed eval with: " <> show err
        Right v' -> (renderPretty v') `shouldBe` (renderPretty v)

evalTests :: Spec
evalTests = describe "evaluate" $
  do
    inferno@(Interpreter {evalExpr, defaultEnv, parseAndInferTypeReps}) <-
      runIO $ (mkInferno builtinPrelude [] :: IO (Interpreter IO TestCustomValue))
    let shouldEvaluateInEnvTo implEnv str (v :: Value TestCustomValue IO) =
          it ("\"" <> unpack str <> "\" should evaluate to " <> (unpack $ renderPretty v)) $ do
            case parseAndInferTypeReps str of
              Left err -> expectationFailure $ show err
              Right ast ->
                evalExpr defaultEnv implEnv ast >>= \case
                  Left err -> expectationFailure $ "Failed eval with: " <> show err
                  Right v' -> (renderPretty v') `shouldBe` (renderPretty v)
    let shouldEvaluateTo = shouldEvaluateInEnvTo Map.empty
    let shouldThrowRuntimeError str merr =
          it ("\"" <> unpack str <> "\" should throw a runtime error") $ do
            case parseAndInferTypeReps str of
              Left err -> expectationFailure $ show err
              Right ast ->
                evalExpr defaultEnv Map.empty ast >>= \case
                  Left err' -> case merr of
                    Nothing -> pure ()
                    Just err -> err' `shouldBe` err
                  Right _ -> expectationFailure $ "Should not evaluate."

    shouldEvaluateTo "3" $ VDouble 3
    shouldEvaluateTo "-3" $ VDouble (-3)
    shouldEvaluateTo "-(-3)" $ VDouble 3
    shouldEvaluateTo "3+4" $ VDouble 7
    shouldEvaluateTo "3.0" $ VDouble 3.0
    shouldEvaluateTo "3.0-2" $ VDouble 1.0
    shouldEvaluateTo "3.0/2" $ VDouble 1.5
    shouldEvaluateTo "(+) 3.0 1.0" $ VDouble 4.0
    -- Reciprocals
    shouldEvaluateTo "3.14 * recip 3.14" $ VDouble 1.0
    -- Power
    shouldEvaluateTo "1.4 ** 2.5" $ VDouble (1.4 ** 2.5)
    shouldEvaluateTo "exp 0" $ VDouble 1.0
    shouldEvaluateTo "exp (ln 1)" $ VDouble 1.0
    -- Logs
    shouldEvaluateTo "log 10" $ VDouble 1.0
    shouldEvaluateTo "logBase 10 100" $ VDouble 2.0
    shouldEvaluateTo "ln (exp 1)" $ VDouble 1.0
    -- Square root
    shouldEvaluateTo "sqrt 1.425" $ VDouble (sqrt 1.425)
    shouldEvaluateTo "sqrt (-1.425)" $ VDouble (sqrt (-1.425))
    -- Negation
    shouldEvaluateTo "let x = 1425 in -x" $ VDouble (-1425)
    shouldEvaluateTo "let x = 1.425 in -x" $ VDouble (-1.425)
    -- Absolute value
    shouldEvaluateTo "abs 1425" $ VDouble 1425
    shouldEvaluateTo "abs (-1425)" $ VDouble 1425
    shouldEvaluateTo "abs 14.25" $ VDouble 14.25
    shouldEvaluateTo "abs (-14.25)" $ VDouble 14.25
    -- Modulus
    shouldEvaluateTo "1425 % 5" $ VInt 0
    shouldEvaluateTo "1426 % 5" $ VInt 1
    shouldEvaluateTo "-3 % 5" $ VInt 2
    -- Floor and ceiling
    shouldEvaluateTo "floor 1425" $ VInt 1425
    shouldEvaluateTo "floor (-1425)" $ VInt (-1425)
    shouldEvaluateTo "floor 14.25" $ VInt 14
    shouldEvaluateTo "floor (-14.25)" $ VInt (-15)
    shouldEvaluateTo "ceiling 1425" $ VInt 1425
    shouldEvaluateTo "ceiling (-1425)" $ VInt (-1425)
    shouldEvaluateTo "ceiling 14.25" $ VInt 15
    shouldEvaluateTo "ceiling (-14.25)" $ VInt (-14)
    -- Rounding
    shouldEvaluateTo "round 1425" $ VInt 1425
    shouldEvaluateTo "round (-1425)" $ VInt (-1425)
    shouldEvaluateTo "round 14.25" $ VInt 14
    shouldEvaluateTo "round 14.55" $ VInt 15
    shouldEvaluateTo "round (-14.25)" $ VInt (-14)
    shouldEvaluateTo "round (-14.55)" $ VInt (-15)
    -- TODO fix type inference here? Check types inferred
    shouldEvaluateTo "roundTo 0 1.72839" $ VDouble 2.0
    shouldEvaluateTo "roundTo 1 1.72839" $ VDouble 1.7
    shouldEvaluateTo "roundTo 2 1.72839" $ VDouble 1.73
    shouldEvaluateTo "roundTo 3 1.72839" $ VDouble 1.728
    shouldEvaluateTo "roundTo 4 1.72839" $ VDouble 1.7284
    shouldEvaluateTo "roundTo 5 1.72839" $ VDouble 1.72839
    shouldEvaluateTo "truncate 1425" $ VInt 1425
    shouldEvaluateTo "truncate (-1425)" $ VInt (-1425)
    shouldEvaluateTo "truncate 14.25" $ VInt 14
    shouldEvaluateTo "truncate 14.55" $ VInt 14
    shouldEvaluateTo "truncate (-14.25)" $ VInt (-14)
    shouldEvaluateTo "truncate (-14.55)" $ VInt (-14)
    shouldEvaluateTo "truncateTo 0 1.72839" $ VDouble 1.0
    shouldEvaluateTo "truncateTo 1 1.72839" $ VDouble 1.7
    shouldEvaluateTo "truncateTo 2 1.72839" $ VDouble 1.72
    shouldEvaluateTo "truncateTo 3 1.72839" $ VDouble 1.728
    shouldEvaluateTo "truncateTo 4 1.72839" $ VDouble 1.7283
    -- Limit
    shouldEvaluateTo "limit 1.72 9.32 (-23.4)" $ VDouble 1.72
    shouldEvaluateTo "limit 1.72 9.32 3.4" $ VDouble 3.4
    shouldEvaluateTo "limit 1.72 9.32 9.32" $ VDouble 9.32
    shouldEvaluateTo "limit 1.72 9.32 233.4" $ VDouble 9.32
    -- Trigonometry
    shouldEvaluateTo "(sin 1.87) ** 2.0 + (cos 1.87) ** 2.0" $ VDouble 1.0
    shouldEvaluateTo "(cosh 1.87) ** 2.0 - (sinh 1.87) ** 2.0" $ VDouble 1.0
    shouldEvaluateTo "tanh 1.87" $ VDouble (sinh 1.87 / cosh 1.87)
    shouldEvaluateTo "truncateTo 4 ((sin 1.87 / (cos 1.87)) - tan 1.87)" $ VDouble 0.0
    shouldEvaluateTo "truncateTo 4 (sin (2 * pi))" $ VDouble 0.0
    shouldEvaluateTo "arcSin (sin 1.02) - 1.02 < 1e-9" $ vTrue
    shouldEvaluateTo "arcCos (cos 1.02) - 1.02 < 1e-9" $ vTrue
    shouldEvaluateTo "arcTan (tan 1.02) - 1.02 < 1e-9" $ vTrue
    -- Booleans
    shouldEvaluateTo "#true" vTrue
    shouldEvaluateTo "!#true" vFalse
    shouldEvaluateTo "!(!#true)" vTrue
    shouldEvaluateTo "#false && #false" vFalse
    shouldEvaluateTo "#true && #false" vFalse
    shouldEvaluateTo "#true && #true" vTrue
    shouldEvaluateTo "#false || #false" vFalse
    shouldEvaluateTo "#true || #false" vTrue
    shouldEvaluateTo "#true || #true" vTrue
    shouldEvaluateTo "#false XOR #false" vFalse
    shouldEvaluateTo "#true XOR #false" vTrue
    shouldEvaluateTo "#true XOR #true" vFalse
    shouldEvaluateTo "#true XOR #true || #true" vTrue
    shouldEvaluateTo "#true || #true XOR #true" vTrue
    shouldEvaluateTo "(#true XOR #true) || #true" vTrue
    shouldEvaluateTo "#true XOR (#true || #true)" vFalse
    shouldEvaluateTo "#true && #false || #true" vTrue
    shouldEvaluateTo "#true || #false && #true" vTrue
    -- Order
    shouldEvaluateTo "1.2 < 8.9" vTrue
    shouldEvaluateTo "-1.2 < -8.9" vFalse
    shouldEvaluateTo "-1.2 < -1.2" vFalse
    shouldEvaluateTo "1.2 > 8.9" vFalse
    shouldEvaluateTo "-1.2 > -8.9" vTrue
    shouldEvaluateTo "-1.2 > -1.2" vFalse
    shouldEvaluateTo "1.2 <= 8.9" vTrue
    shouldEvaluateTo "-1.2 <= (-8.9)" vFalse
    shouldEvaluateTo "-1.2 <= -1.2" vTrue
    shouldEvaluateTo "1.2 >= 8.9" vFalse
    shouldEvaluateTo "-1.2 >= -8.9" vTrue
    shouldEvaluateTo "-1.2 >= -1.2" vTrue
    shouldEvaluateTo "min 1.23 4.33" $ VDouble 1.23
    shouldEvaluateTo "min 11.23 4.33" $ VDouble 4.33
    shouldEvaluateTo "max 1.23 4.33" $ VDouble 4.33
    shouldEvaluateTo "max 11.23 4.33" $ VDouble 11.23
    -- equality is defined for all types, however comparing function types will always yield #false
    shouldEvaluateTo "1.2 == 1.2" vTrue
    shouldEvaluateTo "-1.2 == -1.2" vTrue
    shouldEvaluateTo "1.2 == 3.2" vFalse
    shouldEvaluateTo "1.2 != 1.2" vFalse
    shouldEvaluateTo "-1.2 != -1.2" vFalse
    shouldEvaluateTo "1.2 != 3.2" vTrue
    shouldEvaluateTo "12 == 12" vTrue
    shouldEvaluateTo "-12 == -12" vTrue
    shouldEvaluateTo "12 == 32" vFalse
    shouldEvaluateTo "12 != 12" vFalse
    shouldEvaluateTo "-12 != -12" vFalse
    shouldEvaluateTo "12 != 32" vTrue
    shouldEvaluateTo "(fun x -> x) == (fun x -> x)" vFalse
    shouldEvaluateTo "(fun x -> x) != (fun x -> x)" vTrue
    -- Bits
    shouldEvaluateTo "0x3abc" $ VWord64 15036
    shouldEvaluateTo "testBit 0x1 0" vTrue
    shouldEvaluateTo "testBit 0x1 1" vFalse
    shouldEvaluateTo "testBit 0x2 0" vFalse
    shouldEvaluateTo "testBit (setBit 0x0 3) 3" vTrue
    shouldEvaluateTo "testBit (setBit 0x0 3) 2" vFalse
    shouldEvaluateTo "testBit (clearBit (setBit 0x0 3) 2) 3" vTrue
    shouldEvaluateTo "testBit (clearBit (setBit 0x0 3) 3) 3" vFalse
    shouldEvaluateTo "testBit (complementBit 0x0 3) 3" vTrue
    shouldEvaluateTo "testBit (complementBit (complementBit 0x0 3) 3) 3" vFalse
    shouldEvaluateTo "shift 0x1 3" $ VWord64 8
    shouldEvaluateTo "shift 0x10 (-3)" $ VWord64 2
    shouldEvaluateTo "0x10 && 0x01" $ VWord64 0
    shouldEvaluateTo "0x5 && 0x9" $ VWord64 1
    shouldEvaluateTo "0x5 && 0x6" $ VWord64 4
    shouldEvaluateTo "0x5 || 0x9" $ VWord64 13
    shouldEvaluateTo "0x5 || 0x6" $ VWord64 7
    shouldEvaluateTo "0x5 XOR 0x9" $ VWord64 12
    shouldEvaluateTo "0x5 XOR 0x6" $ VWord64 3
    shouldEvaluateTo "0x10 XOR 0x01" $ VWord64 17
    shouldEvaluateTo "!(toWord16 0x1)" $ VWord64 (fromIntegral (2 ^ (16 :: Integer) - (2 :: Integer)))
    shouldEvaluateTo "toWord16 #true" $ VWord16 1
    shouldEvaluateTo "toWord16 (toWord64 77)" $ VWord16 77
    shouldEvaluateTo "toWord16 (toWord64 (2**17 + 2))" $ VWord16 2
    shouldEvaluateTo "toWord32 (toWord64 77)" $ VWord32 77
    shouldEvaluateTo "toWord32 (toWord64 (2**33 + 5))" $ VWord32 5
    shouldEvaluateTo "toWord64 (toWord16 (2**62 + 1))" $ VWord64 1
    shouldEvaluateTo "fromWord (toWord64 (2**62))" $ VInt (2 ^ (62 :: Int64))
    shouldEvaluateTo "fromWord (toWord32 (2**62 + 2**31))" $ VInt (2 ^ (31 :: Int64))
    shouldEvaluateTo "fromWord (toWord16 (2**31 + 2**3))" $ VInt 8
    shouldEvaluateTo "fromWord #false" $ VInt 0
    shouldEvaluateTo "fromWord #true" $ VInt 1
    -- Arrays
    shouldEvaluateTo "Array.singleton 3.14" $ VArray [VDouble 3.14]
    shouldEvaluateTo "Array.length []" $ VInt 0
    shouldEvaluateTo "Array.length [3.0, 4.0]" $ VInt 2
    shouldEvaluateTo "Array.minimum [3.0, 4.0] ? -999" $ VDouble 3.0
    shouldEvaluateTo "Array.maximum [3.0, 4.0] ? 999" $ VDouble 4.0
    shouldEvaluateTo "Array.average [0.0, 1.0] ? 0" $ VDouble 0.5
    shouldEvaluateTo "Array.median [0.0, 1.0, 2.0] ? 0" $ VDouble 1.0
    shouldEvaluateTo "Array.median [0, 1] ? 0" $ VDouble 0.5
    shouldEvaluateTo "Array.median [] ? 9" $ VDouble 9.0
    shouldEvaluateTo "Array.argmin [3.0, 4.0] ? 1" $ VInt 0
    shouldEvaluateTo "Array.argmax [3.0, 4.0] ? 0" $ VInt 1
    shouldEvaluateTo "Array.argsort [3.0, 1.0, 2.0]" $ VArray [VInt 1, VInt 2, VInt 0]
    shouldEvaluateTo "Array.magnitude [1.0, 2.0, 3.0]" $ VDouble (sqrt (1.0 + 4.0 + 9.0))
    shouldEvaluateTo "Array.norm [1.0, -2.0, 3.0]" $ VDouble (sqrt (1.0 + 4.0 + 9.0))

    shouldEvaluateTo "Array.range 4 3" $ VArray []
    shouldEvaluateTo "Array.range 4 13" $ VArray (map VInt [4 .. 13])
    shouldEvaluateTo "4 .. 13" $ VArray (map VInt [4 .. 13])
    shouldEvaluateTo "Array.map (fun x -> x**2) (Array.range 1 4)" $ VArray (map VInt [1, 4, 9, 16])
    -- The output type depends on the type of the starting value 0:
    shouldEvaluateTo "Array.reduce (fun x y -> x + max 0 y) (round 0) (Array.range (-3) 3)" $ VInt 6
    shouldEvaluateTo "Array.reduce (fun x y -> x + max 0 y) 0 (Array.range (-3) 3)" $ VDouble 6
    shouldEvaluateTo "Array.reduceRight (fun x y -> y + max 0 x) (round 0) (Array.range (-3) 3)" $ VInt 6
    shouldEvaluateTo "Array.reduceRight (fun x y -> y + max 0 x) 0 (Array.range (-3) 3)" $ VDouble 6
    shouldEvaluateTo "(Array.reduce (fun x y -> x + max 0 y) 0 ((-3) .. 3)) == 6" vTrue
    shouldEvaluateTo "(Array.reduce (fun x y -> x + max 0 y) 0 ((-3) .. 3)) == 6.0" vTrue
    shouldEvaluateTo "(Array.reduce (fun x y -> x + max 0 y) 0 ((-3) .. 3)) == (Array.reduceRight (fun x y -> y + max 0 x) 0 ((-3) .. 3))" vTrue
    shouldEvaluateTo "Array.sum [1.0, 2.0, 4.0, 8.0]" $ VDouble 15
    shouldEvaluateTo "open Array in range 4 13" $ VArray (map VInt [4 .. 13])
    shouldEvaluateTo "open Time in Array.sum [seconds 2, hours 5]" $ VEpochTime 18002
    -- Option type
    shouldEvaluateTo "Array.sum (Array.keepSomes [Some 3.0, None, Some 4.0])" $ VDouble 7
    shouldEvaluateTo "Array.findFirstSome [None, Some 3.0, None, Some 4.0]" $ VOne $ VDouble 3
    shouldEvaluateTo "Array.findLastSome [None, Some 3.0, None, Some 4.0]" $ VOne $ VDouble 4
    shouldEvaluateTo "Array.findFirstAndLastSome [None, Some 3.0, None, Some 4.0]" $ VOne $ VTuple [VDouble 3, VDouble 4]
    shouldEvaluateTo "Option.map (fun x -> x + 2) (Some 4.0)" $ VOne $ VDouble 6
    shouldEvaluateTo "Option.map (fun x -> x + 2) None" VEmpty
    shouldEvaluateTo "fromOption 0 (Some 4.0)" $ VDouble 4
    shouldEvaluateTo "fromOption 0.0 None" $ VDouble 0
    shouldEvaluateTo "(Some 4.0) ? 0" $ VDouble 4
    shouldEvaluateTo "None ? 0.0" $ VDouble 0
    shouldEvaluateTo "Option.reduce (fun d -> d + 2) 0.0 (Some 4)" $ VDouble 6
    shouldEvaluateTo "Option.reduce (fun d -> d + 2) 0.0 (Some 4.0)" $ VDouble 6
    shouldEvaluateTo "Option.reduce (fun d -> d + 2) 0 (Some 4.0)" $ VDouble 6
    shouldEvaluateTo "Option.reduce (fun d -> d + 2) 0.0 None" $ VDouble 0
    -- Time
    shouldEvaluateTo "Time.seconds 5" $ VEpochTime 5
    shouldEvaluateTo "Time.minutes 5 == 5 * Time.seconds 60" vTrue
    shouldEvaluateTo "Time.hours 5 == 5 * Time.minutes 60" vTrue
    shouldEvaluateTo "Time.days 5 == 5 * Time.hours 24" vTrue
    shouldEvaluateTo "Time.weeks 5 == 5 * Time.days 7" vTrue
    shouldEvaluateTo "open Time in let ?now = toTime (seconds 4000) in intervalEvery (seconds 4) ?now (?now + seconds 10)" $
      VArray [VEpochTime 4000, VEpochTime 4004, VEpochTime 4008]
    shouldEvaluateTo "open Time in hour (toTime (hours 3 + seconds 400)) == toTime (hours 3)" vTrue
    shouldEvaluateTo "open Time in day (toTime (days 3 + hours 22)) == toTime (days 3)" vTrue
    shouldEvaluateTo "open Time in month (toTime (days 66)) == toTime (days 59)" vTrue
    shouldEvaluateTo "open Time in year (toTime (days 367))" $ VEpochTime (60 * 60 * 24 * 365)
    -- December 1, 2022 1:18:10 AM to January 1, 2022 12:00:00 AM
    shouldEvaluateTo "open Time in year (toTime (seconds 1669857490))" $ VEpochTime 1640995200
    -- December 1, 2022 1:18:10 AM to December 1, 2022 12:00:00 AM
    shouldEvaluateTo "open Time in month (toTime (seconds 1669857490))" $ VEpochTime 1669852800
    shouldEvaluateTo
      "open Time in let ?now = (toTime (seconds 66666)) in secondsBefore ?now 44 == ?now - (seconds 44)"
      vTrue
    shouldEvaluateTo
      "open Time in let ?now = (toTime (minutes 66666)) in minutesBefore ?now 44 == ?now - (minutes 44)"
      vTrue
    shouldEvaluateTo
      "open Time in let ?now = (toTime (hours 66666)) in hoursBefore ?now 44 == ?now - (hours 44)"
      vTrue
    shouldEvaluateTo
      "open Time in let ?now = (toTime (days 66666)) in daysBefore ?now 44 == ?now - (days 44)"
      vTrue
    shouldEvaluateTo
      "open Time in let ?now = (toTime (weeks 66666)) in weeksBefore ?now 44 == ?now - (weeks 44)"
      vTrue
    shouldEvaluateTo
      -- 2 months before 1970-04-11 is 1970-02-11
      "open Time in monthsBefore (toTime (days 100 + hours 22)) 2 == toTime (days 41 + hours 22)"
      vTrue
    shouldEvaluateTo
      -- 3 months before 1970-05-31 is 1970-02-28 because of clipping
      "open Time in monthsBefore (toTime (days 150 + hours 22)) 3 == toTime (days 58 + hours 22)"
      vTrue
    shouldEvaluateTo
      -- 2 years before 1972-04-11 is 1970-04-11
      "open Time in yearsBefore (toTime (days 831 + hours 22)) 2 == toTime (days 100 + hours 22)"
      vTrue
    shouldEvaluateTo
      -- 2 years before 1972-02-29 is 1970-02-28 because of clipping
      "open Time in yearsBefore (toTime (days 789 + hours 22)) 2 == toTime (days 58 + hours 22)"
      vTrue
    shouldEvaluateTo "Time.formatTime (Time.toTime (Time.seconds 0)) \"%H:%M:%S\"" $ VText "00:00:00"
    shouldEvaluateTo "Time.formatTime (Time.toTime (Time.seconds 0)) \"%c\"" $ VText "Thu Jan  1 00:00:00 UTC 1970"
    -- Text
    shouldEvaluateTo "Text.append \"hello \" \"world\"" $ VText "hello world"
    shouldEvaluateTo "Text.length \"hello\"" $ VInt 5
    shouldEvaluateTo "Text.strip \" hello \"" $ VText "hello"
    shouldEvaluateTo "Text.splitAt 5 \"hello world\"" $ VTuple [VText "hello", VText " world"]
    -- Array indexing
    shouldEvaluateTo "Array.get [0, 1, 2] 0" $ VDouble 0
    shouldEvaluateTo "Array.get [0, 1, 2] 1" $ VDouble 1
    shouldEvaluateTo "Array.get [0, 1, 2] 2" $ VDouble 2
    shouldThrowRuntimeError "Array.get [0, 1, 2] (-9)" Nothing
    shouldThrowRuntimeError "Array.get [0, 1, 2] 3" Nothing
    shouldEvaluateTo "[0, 1, 2] !! 0" $ VDouble 0
    shouldEvaluateTo "[0, 1, 2] !! 1" $ VDouble 1
    shouldEvaluateTo "[0, 1, 2] !! 2" $ VDouble 2
    shouldThrowRuntimeError "[0, 1, 2] !! -9" Nothing
    shouldThrowRuntimeError "[0, 1, 2] !! 3" Nothing
    shouldEvaluateTo "Array.getOpt [0, 1, 2] 0" $ VOne $ VDouble 0
    shouldEvaluateTo "Array.getOpt [0, 1, 2] 1" $ VOne $ VDouble 1
    shouldEvaluateTo "Array.getOpt [0, 1, 2] 2" $ VOne $ VDouble 2
    shouldEvaluateTo "Array.getOpt [0, 1, 2] (-9)" VEmpty
    shouldEvaluateTo "Array.getOpt [0, 1, 2] 3" VEmpty
    shouldEvaluateTo "[0, 1, 2] !? 0" $ VOne $ VDouble 0
    shouldEvaluateTo "[0, 1, 2] !? 1" $ VOne $ VDouble 1
    shouldEvaluateTo "[0, 1, 2] !? 2" $ VOne $ VDouble 2
    shouldEvaluateTo "[0, 1, 2] !? -9" VEmpty
    shouldEvaluateTo "[0, 1, 2] !? 3" VEmpty
    -- Array pattern matching
    shouldEvaluateTo "match [] with { | [] -> 0 | [x] -> 1 | [x, y] -> 2 | _ -> 3 }" $ VDouble 0
    shouldEvaluateTo "match [1] with { | [] -> 0 | [x] -> 1 | [x, y] -> 2 | _ -> 3 }" $ VDouble 1
    shouldEvaluateTo "match [1, 2] with { | [] -> 0 | [x] -> 1 | [x, y] -> 2 | _ -> 3 }" $ VDouble 2
    shouldEvaluateTo "match [1, 2, 3] with { | [] -> 0 | [x] -> 1 | [x, y] -> 2 | _ -> 3 }" $ VDouble 3
    shouldEvaluateTo "match [1, 3, 3] with { | [1] -> 2 | _ -> 3 }" $ VDouble 3
    shouldEvaluateTo "match [1, 3] with { | [1, _] -> 2 | _ -> 3 }" $ VDouble 2
    shouldEvaluateTo "match [1.2, 3, 3] with { | [x, y, z] -> 2*x+3*y+z | _ -> 3 }" $ VDouble 14.4
    shouldEvaluateTo "(fun a -> match a with { | [x, y, z] -> truncateTo x 1.1 | _ -> 3 }) [1, 2, 3]" $ VDouble 1.1
    -- Tuple
    shouldEvaluateTo "fst (1, 0) == snd (0, 1)" $ vTrue
    shouldEvaluateTo "zip [1, 2, 3] [4, 5] == [(1, 4), (2, 5)]" $ vTrue
    shouldEvaluateTo "zip [1, 2] [\"a\", \"b\"] == [(1,\"a\"),(2,\"b\")]" $ vTrue
    shouldEvaluateTo "zip [1] [\"a\", \"b\"] == [(1,\"a\")]" $ vTrue
    shouldEvaluateTo "zip [1, 2] [\"a\"] == [(1,\"a\")]" $ vTrue
    shouldEvaluateTo "zip [] [1, 2] == []" $ vTrue
    shouldEvaluateTo "zip [1, 2] [] == []" $ vTrue
    -- Type annotations
    shouldEvaluateTo "let x : int = 2 in x" $ VInt 2
    shouldEvaluateTo "let x : double = 2 in x" $ VDouble 2
    -- Miscellaneous
    shouldEvaluateTo "Array.map ((Text.append \"a\") << (Text.append \"b\")) [\"0\", \"1\"] == [\"ab0\", \"ab1\"]" $ vTrue
    shouldEvaluateTo "\"0\" |> Text.append \"a\" |> Text.append \"b\" == \"ba0\"" $ vTrue
    shouldEvaluateTo "\"hello world\"" $ VText "hello world"
    shouldEvaluateInEnvTo
      (Map.fromList [(ExtIdent $ Right "x", VInt 5)])
      "?x + 2"
      (VDouble 7)
    shouldEvaluateInEnvTo
      (Map.fromList [(ExtIdent $ Right "x", VInt 5)])
      "let f = fun x -> ?x + 2 in f 0"
      (VDouble 7)
    shouldEvaluateTo "let ?x = 3.2 in ?x + 2" $ VDouble 5.2
    shouldEvaluateTo "let x = 3.2 in x + 2" $ VDouble 5.2
    shouldEvaluateTo "if #true then Some 2.0 else None" $ VOne (VDouble 2)
    shouldEvaluateTo "match #true with { | #true -> #false | _ -> #true}" vFalse
    shouldEvaluateTo "match 3.9 - 2.2 with { 0.0 -> #false | _ -> #true}" vTrue
    shouldEvaluateTo "match 1 < 2 with { | #true -> 1.1 | #false -> 2.2}" $ VDouble 1.1
    shouldEvaluateTo "match 1 > 2 with { | #true -> 1.1 | #false -> 2.2}" $ VDouble 2.2
    shouldEvaluateTo "`hello ${Array.range 1 10}`" $ VText "hello [1,2,3,4,5,6,7,8,9,10]"
    shouldEvaluateTo "`${id}`" $ VText "<<function>>"
    shouldEvaluateTo "`hello\nworld${`I am ${\"nested\"}`}`" $ VText "hello\nworldI am nested"
    shouldEvaluateTo "[x | x <- 1 .. 10]" $ VArray (map VInt [1 .. 10])
    shouldEvaluateTo "[x | x <- 1 .. 10, if x % 2 == 0]" $ VArray (map VInt [2, 4, 6, 8, 10])
    shouldThrowRuntimeError "assert #false in ()" $ Just AssertionFailed
    shouldEvaluateTo "assert #true in ()" $ VTuple []

    runtimeTypeRepsTests inferno

    evalInMonadTest
  where
    vTrue = VEnum enumBoolHash (Ident "true")
    vFalse = VEnum enumBoolHash (Ident "false")

-------------------------------------------------------------------------------
-- Test running interpreter in a custom (reader) monad:
-------------------------------------------------------------------------------

data TestEnv = TestEnv {cache :: Int64}

cachedGet :: (MonadReader TestEnv m, MonadThrow m) => Value TestCustomValue (ImplEnvM m TestCustomValue)
cachedGet =
  VFun $ \_ -> do
    TestEnv {cache} <- liftImplEnvM $ ask
    pure $ VInt cache

-- Since this is a test, we build a prelude from scratch, instead of extending the
-- builtin/core Inferno prelude from Inferno.Module.Prelude.
-- To keep the rest of the code happy, we need to include a dummy Base module.
evalInMonadPrelude :: Prelude (ReaderT TestEnv IO) TestCustomValue
evalInMonadPrelude =
  [builtinPreludeQuoter|
module Base
  zero : int := 0;

module EvalInMonad
  cachedGet : () -> int := ###!cachedGet###;
|]

evalInMonadTest :: Spec
evalInMonadTest = do
  let testEnv = TestEnv {cache = 4}

  Interpreter {evalExpr, defaultEnv, parseAndInferTypeReps} <-
    runIO $
      flip runReaderT testEnv $
        (mkInferno evalInMonadPrelude [] :: ReaderT TestEnv IO (Interpreter (ReaderT TestEnv IO) TestCustomValue))

  let shouldEvaluateInEnvTo implEnv str (v :: Value TestCustomValue IO) =
        it ("\"" <> unpack str <> "\" should evaluate to " <> (unpack $ renderPretty v)) $ do
          case parseAndInferTypeReps str of
            Left err -> expectationFailure $ show err
            Right ast -> do
              res <- flip runReaderT testEnv $ evalExpr defaultEnv implEnv ast
              case res of
                Left err -> expectationFailure $ "Failed eval with: " <> show err
                Right v' -> (renderPretty v') `shouldBe` (renderPretty v)
  let shouldEvaluateTo = shouldEvaluateInEnvTo Map.empty

  describe "evaluate in custom monad" $ do
    shouldEvaluateTo "EvalInMonad.cachedGet ()" $ VInt 4
