module Main (main) where

import Data.Bifunctor (Bifunctor (second))
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Inferno.Types.Syntax
import Inferno.Utils.Prettyprinter (renderPretty)
import Test.Hspec (Example (Arg), Expectation, SpecWith, describe, hspec, it, shouldBe)
import Text.Megaparsec (initialPos)

main :: IO ()
main = do
  hspec $ do
    describe "unused variables" $ do
      unusedVarsShouldBe
        -- let x = 1 in 2.2
        (Let () () (Expl (ExtIdent (Right "x"))) () (Lit () (LInt 1)) () (Lit () (LDouble 2.2)))
        ["x"]

      unusedVarsShouldBe
        -- let x = 1 in let y = x + 2 in 2.2
        (Let () () (Expl (ExtIdent (Right "x"))) () (Lit () (LInt 1)) () (Let () () (Expl (ExtIdent (Right "y"))) () (Op (Var () () LocalScope (Expl (ExtIdent (Right "x")))) () () (9, LeftFix) LocalScope (Ident {unIdent = "+"}) (Lit () (LInt 2))) () (Lit () (LDouble 2.2))))
        ["y"]

      unusedVarsShouldBe
        -- let x = 1 in if let y = x + 2 in #true then 3.3 else let z = 3 in 4
        (Let () () (Expl (ExtIdent (Right "x"))) () (Lit () (LInt 1)) () (If () (Let () () (Expl (ExtIdent (Right "y"))) () (Op (Var () () LocalScope (Expl (ExtIdent (Right "x")))) () () (9, LeftFix) LocalScope (Ident {unIdent = "+"}) (Lit () (LInt 2))) () (Enum () () LocalScope (Ident {unIdent = "true"}))) () (Lit () (LDouble 3.3)) () (Let () () (Expl (ExtIdent (Right "z"))) () (Lit () (LInt 3)) () (Lit () (LInt 4)))))
        ["y", "z"]

      unusedVarsShouldBe
        -- let x = 1 in if let y = 2 in #true then 3.3 + x else let z = 3 in 4
        (Let () () (Expl (ExtIdent (Right "x"))) () (Lit () (LInt 1)) () (If () (Let () () (Expl (ExtIdent (Right "y"))) () (Lit () (LInt 2)) () (Enum () () LocalScope (Ident {unIdent = "true"}))) () (Op (Lit () (LDouble 3.3)) () () (9, LeftFix) LocalScope (Ident {unIdent = "+"}) (Var () () LocalScope (Expl (ExtIdent (Right "x"))))) () (Let () () (Expl (ExtIdent (Right "z"))) () (Lit () (LInt 3)) () (Lit () (LInt 4)))))
        ["y", "z"]

      unusedVarsShouldBe
        -- let x = 1 in if let y = 2 in #true then 3.3 else let z = 3 + x in 4
        (Let () () (Expl (ExtIdent (Right "x"))) () (Lit () (LInt 1)) () (If () (Let () () (Expl (ExtIdent (Right "y"))) () (Lit () (LInt 2)) () (Enum () () LocalScope (Ident {unIdent = "true"}))) () (Lit () (LDouble 3.3)) () (Let () () (Expl (ExtIdent (Right "z"))) () (Op (Lit () (LInt 3)) () () (9, LeftFix) LocalScope (Ident {unIdent = "+"}) (Var () () LocalScope (Expl (ExtIdent (Right "x"))))) () (Lit () (LInt 4)))))
        ["y", "z"]

      -- These tests should ideally test the positions as well:

      unusedVarsShouldBe
        -- let x = 1 in let x = 2 + x in 3
        (Let () () (Expl (ExtIdent (Right "x"))) () (Lit () (LInt 1)) () (Let () () (Expl (ExtIdent (Right "x"))) () (Op (Lit () (LInt 2)) () () (9, LeftFix) LocalScope (Ident {unIdent = "+"}) (Var () () LocalScope (Expl (ExtIdent (Right "x"))))) () (Lit () (LInt 3))))
        ["x"]
      unusedVarsShouldBe
        -- let x = 1 in let x = 2 in 3 + x
        (Let () () (Expl (ExtIdent (Right "x"))) () (Lit () (LInt 1)) () (Let () () (Expl (ExtIdent (Right "x"))) () (Lit () (LInt 2)) () (Op (Lit () (LInt 3)) () () (9, LeftFix) LocalScope (Ident {unIdent = "+"}) (Var () () LocalScope (Expl (ExtIdent (Right "x")))))))
        ["x"]
      unusedVarsShouldBe
        -- let x = 1 in let x = 2 + x in 3 + x
        (Let () () (Expl (ExtIdent (Right "x"))) () (Lit () (LInt 1)) () (Let () () (Expl (ExtIdent (Right "x"))) () (Op (Lit () (LInt 2)) () () (9, LeftFix) LocalScope (Ident {unIdent = "+"}) (Var () () LocalScope (Expl (ExtIdent (Right "x"))))) () (Op (Lit () (LInt 3)) () () (9, LeftFix) LocalScope (Ident {unIdent = "+"}) (Var () () LocalScope (Expl (ExtIdent (Right "x")))))))
        []
  where
    unusedVarsShouldBe :: Expr () () -> [Text] -> SpecWith (Arg Expectation)
    unusedVarsShouldBe e vs =
      it (unpack (renderPretty e) <> " == " <> show vs) $ do
        let e' = second (const $ initialPos "") e
        let vs' = Set.map (\(x, _, _) -> x) $ unusedVars e'
        vs' `shouldBe` Set.fromList vs
