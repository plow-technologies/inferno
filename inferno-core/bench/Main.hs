{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.Text (Text)
import qualified Data.Text as Text
import Inferno.Core
  ( Interpreter (Interpreter, parseAndInferTypeReps),
    mkInferno,
  )
import qualified Inferno.Module.Prelude as Prelude
import NeatInterpolation (trimming)

main :: IO ()
main = do
  Interpreter{parseAndInferTypeReps} <-
    mkInferno (Prelude.builtinModules @IO @()) []
  defaultMain
    [ bgroup
        "wide"
        [ bench "100" . whnf parseAndInferTypeReps $ genWide 100
        , bench "500" . whnf parseAndInferTypeReps $ genWide 500
        , bench "1000" . whnf parseAndInferTypeReps $ genWide 1000
        ]
    , bgroup
        "deep"
        [ bench "25" . whnf parseAndInferTypeReps $ genDeep 25
        , bench "50" . whnf parseAndInferTypeReps $ genDeep 50
        , bench "100" . whnf parseAndInferTypeReps $ genDeep 100
        ]
    ]

-- Generate a script with `n` `let`-bindings chaining simple arithmetic.
-- Stresses environment growth, scope resolution, and substitution application.
genWide :: Int -> Text
genWide n = foldMap binding [0 .. n - 1] <> mconcat ["x", tshow $ n - 1]
  where
    binding :: Int -> Text
    binding 0 = "let x0 = 1.0 in\n"
    binding i
      | even i =
          withNewline [trimming|let x$xi = x$xp + x$xh in|]
      | otherwise =
          withNewline [trimming|let x$xi = x$xp + $xi.0 in|]
      where
        xi, xp, xh :: Text
        xi = tshow i
        xp = tshow $ i - 1
        xh = tshow $ i `div` 2

-- Generate a script with `n` blocks (~6 bindings each) exercising lambdas,
-- comprehensions, `Array.reduce`, records, `if`/`then`/`else`, `Some`/`None`,
-- and `match`. Variables chain across blocks so the type environment grows
-- realistically.
genDeep :: Int -> Text
genDeep n = mconcat [foldMap block [0 .. n - 1], "acc", tshow (n - 1)]
  where
    prev :: Int -> Text
    prev = \case
      0 -> "1.0"
      i -> "acc" <> tshow (i - 1)

    block :: Int -> Text
    block i =
      withNewline
        [trimming|
          let f$si = fun x -> x * $c + $p in
          let a$si = [f$si x | x <- [1.0, 2.0, 3.0, 4.0, 5.0]] in
          let t$si = Array.reduce (+) 0.0 a$si in
          let r$si = {a = t$si; b = f$si 10.0; c = Array.length a$si} in
          let v$si =
            match (if t$si > 0.0 then Some r$si.a else None) with {
              | Some y -> y + 1.0
              | None -> 0.0
            }
          in
          let acc$si = v$si in
        |]
      where
        si, c, p :: Text
        si = tshow i
        c = tshow (i + 1) <> ".1"
        p = prev i

tshow :: Int -> Text
tshow = Text.pack . show

-- The `trimming` QQ strips all leading/trailing whitespace chars, including
-- newlines, so we need to add them back in to generate the script blocks
withNewline :: Text -> Text
withNewline = (<> "\n")
