{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Parse.Spec where

import Control.Monad (void)
import Data.Bifunctor (second)
import Data.Functor.Foldable (ana, project)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Inferno.Instances.Arbitrary ()
import Inferno.Module.Prelude (ModuleMap, baseOpsTable, builtinModules, builtinModulesOpsTable)
import Inferno.Parse (parseExpr, prettyError)
import Inferno.Types.Syntax
  ( BlockUtils (removeComments),
    Expr (..),
    ExtIdent (..),
    IStr (..),
    Ident (Ident),
    ImplExpl (..),
    InfixFixity (..),
    Lit (..),
    ModuleName (..),
    Pat (..),
    Scoped (..),
    SomeIStr (..),
    TList (..),
  )
import Inferno.Utils.Prettyprinter (renderPretty)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( Property,
    Testable (property),
    counterexample,
    within,
    (===),
  )
import Text.Pretty.Simple (pShow)

prelude :: ModuleMap IO ()
prelude = builtinModules

normalizePat :: Pat h a -> Pat h a
normalizePat = ana $ \case
  PTuple p1 xs p2 -> project $ PTuple p1 (fmap (\(e, _) -> (normalizePat e, Nothing)) xs) p2
  x -> project x

normalizeExpr :: Expr () a -> Expr () a
normalizeExpr = ana $ \case
  PreOp pos hsh prec LocalScope (Ident "-") e -> case normalizeExpr e of
    Lit l' (LInt x) -> project $ Lit l' $ LInt $ -x
    Lit l' (LDouble x) -> project $ Lit l' $ LDouble $ -x
    PreOp _ _ _ LocalScope (Ident "-") e' -> project e'
    e' -> project $ PreOp pos hsh prec LocalScope (Ident "-") e'
  Tuple p1 xs p2 -> project $ Tuple p1 (fmap (\(e, _) -> (normalizeExpr e, Nothing)) xs) p2
  Record p1 xs p2 -> project $ Record p1 (fmap (\(f, e, _) -> (f, normalizeExpr e, Nothing)) xs) p2
  -- Convert RecordField back to scoped Var because that's how parser parses it:
  RecordField p (Ident r) (Ident f) -> project $ Var p () (Scope $ ModuleName r) $ Expl $ ExtIdent $ Right f
  Array p1 xs p2 -> project $ Array p1 (fmap (\(e, _) -> (normalizeExpr e, Nothing)) xs) p2
  ArrayComp p1 e_body p2 args e_cond p3 ->
    project $
      ArrayComp
        p1
        (normalizeExpr e_body)
        p2
        (fmap (\(p4, x, p5, e, _) -> (p4, x, p5, normalizeExpr e, Nothing)) args)
        (fmap (second normalizeExpr) e_cond)
        p3
  Bracketed _ e _ -> project $ normalizeExpr e
  Op e1 p1 h (_, fix) modNm i e2 -> project $ Op (normalizeExpr e1) p1 h (0, fix) modNm i (normalizeExpr e2)
  Case p1 e_case p2 patExprs p3 -> project $ Case p1 (normalizeExpr e_case) p2 (fmap (\(p4, p, p5, e) -> (p4, normalizePat p, p5, normalizeExpr e)) patExprs) p3
  x -> project x

(<?>) :: Testable p => p -> Text -> Property
(<?>) = flip (counterexample . unpack)

infixl 2 <?>

parsingTests :: Spec
parsingTests = describe "pretty printing/parsing" $ do
  prop "parseExpr and pretty are inverse up to normalizeExpr" $
    \(x :: Expr () ()) ->
      within 10000000 $
        case parseExpr (baseOpsTable prelude) (builtinModulesOpsTable prelude) [] (renderPretty x) of
          Left err ->
            property False
              <?> ( "Pretty: \n"
                      <> renderPretty x
                      <> "\nParse error:\n"
                      <> pack (prettyError $ fst $ NonEmpty.head err)
                  )
          Right (res, _comments) ->
            (normalizeExpr (removeComments x) === normalizeExpr (void res))
              <?> ( "Pretty: \n"
                      <> renderPretty x
                      <> "\nParsed: \n"
                      <> toStrict (pShow res)
                      <> "\nParsed pretty: \n"
                      <> renderPretty res
                  )

  describe "parsing literals" $ do
    shouldSucceedFor "0" $ Lit () (LInt 0)
    shouldSucceedFor "0.0" $ Lit () (LDouble 0)
    shouldSucceedFor "3" $ Lit () (LInt 3)
    shouldSucceedFor "3.1415" $ Lit () (LDouble 3.1415)
    shouldSucceedFor "0xff" $ Lit () (LHex 255)
    shouldSucceedFor "0Xff" $ Lit () (LHex 255)
    shouldSucceedFor "0xFf" $ Lit () (LHex 255)
    shouldSucceedFor "0XFF" $ Lit () (LHex 255)
    shouldSucceedFor "0x123456789abcdef" $ Lit () (LHex 81985529216486895)
    shouldFailFor "0x3.14"
    shouldSucceedFor "\"0XFF\"" $ Lit () (LText "0XFF")
    shouldSucceedFor "\"0X\\nFF\"" $ Lit () (LText "0X\nFF")
    shouldSucceedFor "\"0X\\\\nFF\"" $ Lit () (LText "0X\\nFF")
    shouldFailFor "\"0X\nFF\""

  describe "parsing interpolated strings" $ do
    shouldSucceedFor "``" $ InterpolatedString () (SomeIStr ISEmpty) ()
    shouldSucceedFor "`hello\nworld`" $ InterpolatedString () (SomeIStr (ISStr "hello\nworld" ISEmpty)) ()
    shouldSucceedFor "`${1}`" $ InterpolatedString () (SomeIStr (ISExpr ((), Lit () (LInt 1), ()) ISEmpty)) ()
    shouldSucceedFor "`hello\nworld${1}`" $ InterpolatedString () (SomeIStr (ISStr "hello\nworld" (ISExpr ((), Lit () (LInt 1), ()) ISEmpty))) ()
    shouldSucceedFor "`hello\nworld${\"!\"}`" $ InterpolatedString () (SomeIStr (ISStr "hello\nworld" (ISExpr ((), Lit () (LText "!"), ()) ISEmpty))) ()
    shouldSucceedFor "`hello\nworld${`I am ${\"nested\"}`}`" $
      InterpolatedString () (SomeIStr (ISStr "hello\nworld" (ISExpr ((), InterpolatedString () (SomeIStr (ISStr "I am " (ISExpr ((), Lit () (LText "nested"), ()) ISEmpty))) (), ()) ISEmpty))) ()
    shouldFailFor "`hello\nworld${}`"

  describe "parsing negation" $ do
    shouldSucceedFor "-3" $ PreOp () () 19 LocalScope (Ident "-") (Lit () (LInt 3))
    shouldSucceedFor "(-3)" $ Bracketed () (PreOp () () 19 LocalScope (Ident "-") (Lit () (LInt 3))) ()
    shouldFailFor "--3"
    shouldSucceedFor "-x" $ PreOp () () 19 LocalScope (Ident "-") (Var () () LocalScope (Expl (ExtIdent $ Right "x")))
    shouldSucceedFor "(-(-x))" $ Bracketed () (PreOp () () 19 LocalScope (Ident "-") (Bracketed () (PreOp () () 19 LocalScope (Ident "-") (Var () () LocalScope (Expl (ExtIdent $ Right "x")))) ())) ()
    shouldSucceedFor "5+(-3)" $ Op (Lit () (LInt 5)) () () (9, LeftFix) LocalScope (Ident "+") (Bracketed () (PreOp () () 19 LocalScope (Ident "-") (Lit () (LInt 3))) ())
    shouldSucceedFor "5+-3" $ Op (Lit () (LInt 5)) () () (9, LeftFix) LocalScope (Ident "+") (PreOp () () 19 LocalScope (Ident "-") (Lit () (LInt 3)))
    shouldSucceedFor "-3+5" $ Op (PreOp () () 19 LocalScope (Ident "-") (Lit () (LInt 3))) () () (9, LeftFix) LocalScope (Ident "+") (Lit () (LInt 5))
    shouldSucceedFor "(-3)+5" $ Op (Bracketed () (PreOp () () 19 LocalScope (Ident "-") (Lit () (LInt 3))) ()) () () (9, LeftFix) LocalScope (Ident "+") (Lit () (LInt 5))

  describe "parsing variables" $ do
    shouldSucceedFor "x" $ Var () () LocalScope (Expl (ExtIdent $ Right "x"))
    shouldSucceedFor "A.x" $ Var () () (Scope (ModuleName "A")) (Expl (ExtIdent $ Right "x"))
    shouldSucceedFor "x127652" $ Var () () LocalScope (Expl (ExtIdent $ Right "x127652"))
    shouldSucceedFor "X12aaAA" $ Var () () LocalScope (Expl (ExtIdent $ Right "X12aaAA"))
    shouldSucceedFor "X_12aaAA" $ Var () () LocalScope (Expl (ExtIdent $ Right "X_12aaAA"))
    shouldSucceedFor "X__" $ Var () () LocalScope (Expl (ExtIdent $ Right "X__"))
    shouldFailFor "_x"
    shouldFailFor "let _ = () in ()"
    shouldFailFor "let _x = () in ()"
    shouldSucceedFor "fun _x -> ()" $ Lam () (((), Nothing) NonEmpty.:| []) () (Tuple () TNil ())

  describe "parsing implicit variables" $ do
    let letImpl x = Let () () (Impl (ExtIdent $ Right x)) () (Tuple () TNil ()) () (Tuple () TNil ())
    shouldSucceedFor "let ?X__ = () in ()" $ letImpl "X__"
    shouldSucceedFor "let ?x = () in ()" $ letImpl "x"
    shouldSucceedFor "let ?x_y = () in ()" $ letImpl "x_y"
    shouldFailFor "let ?_ = () in ()"
    shouldFailFor "let ?x-y = () in ()"

  describe "parsing tuples" $ do
    shouldSucceedFor "()" $ Tuple () TNil ()
    shouldSucceedFor "(None, None)" $ Tuple () (TCons (Empty (), Just ()) (Empty (), Nothing) []) ()
    shouldSucceedFor "(None)" $ Bracketed () (Empty ()) ()

  describe "parsing arrays" $ do
    shouldSucceedFor "[]" $ Array () [] ()
    shouldSucceedFor "[None, None]" $ Array () [(Empty (), Just ()), (Empty (), Nothing)] ()

  describe "parsing records" $ do
    let r = Record () [(Ident "name", Lit () (LText "Zaphod"), Just ()), (Ident "age", Lit () (LInt 391), Nothing)] ()
    shouldSucceedFor "{}" $ Record () [] ()
    shouldSucceedFor "{name = \"Zaphod\"; age = 391}" r
    -- Records are parsed as Var, converted to RecordField later in pinExpr:
    let varRecordAccess = Var () () (Scope (ModuleName "r")) (Expl (ExtIdent (Right "age")))
    shouldSucceedFor "let r = {name = \"Zaphod\"; age = 391} in r.age" $
      Let () () (Expl $ ExtIdent $ Right "r") () r () varRecordAccess

  describe "parsing infix operators" $ do
    shouldSucceedFor "2*3+7/2" $
      Op
        (Op (Lit () (LInt 2)) () () (10, LeftFix) LocalScope (Ident "*") (Lit () (LInt 3)))
        ()
        ()
        (9, LeftFix)
        LocalScope
        (Ident "+")
        (Op (Lit () (LInt 7)) () () (10, LeftFix) LocalScope (Ident "/") (Lit () (LInt 2)))
    shouldSucceedFor "2*(3+7)/2" $
      Op
        ( Op
            (Lit () (LInt 2))
            ()
            ()
            (10, LeftFix)
            LocalScope
            (Ident "*")
            (Bracketed () (Op (Lit () (LInt 3)) () () (9, LeftFix) LocalScope (Ident "+") (Lit () (LInt 7))) ())
        )
        ()
        ()
        (10, LeftFix)
        LocalScope
        (Ident "/")
        (Lit () (LInt 2))
    shouldSucceedFor "2*3*4" $
      Op
        (Op (Lit () (LInt 2)) () () (10, LeftFix) LocalScope (Ident "*") (Lit () (LInt 3)))
        ()
        ()
        (10, LeftFix)
        LocalScope
        (Ident "*")
        (Lit () (LInt 4))
    -- should this parse or should <,<=,>,>= be the same precedence level as == ?
    shouldSucceedFor "2>3==3<2" $
      Op
        (Op (Lit () (LInt 2)) () () (7, NoFix) LocalScope (Ident ">") (Lit () (LInt 3)))
        ()
        ()
        (6, NoFix)
        LocalScope
        (Ident "==")
        (Op (Lit () (LInt 3)) () () (7, NoFix) LocalScope (Ident "<") (Lit () (LInt 2)))
    shouldFailFor "2==3==4"

  describe "parsing case statements" $ do
    shouldFailFor "match () with {}"
    shouldSucceedFor "match () with { () -> ()}" $ Case () (Tuple () TNil ()) () (((), PTuple () TNil (), (), Tuple () TNil ()) NonEmpty.:| []) ()
    shouldSucceedFor "match () with { | () -> ()}" $ Case () (Tuple () TNil ()) () (((), PTuple () TNil (), (), Tuple () TNil ()) NonEmpty.:| []) ()

  describe "parsing assertions" $ do
    shouldFailFor "assert #false"
    shouldSucceedFor "assert #false in ()" $ Assert () (Enum () () LocalScope (Ident "false")) () (Tuple () TNil ())

  describe "parsing array builder" $ do
    shouldSucceedFor "[() | x <- someList]" $
      ArrayComp
        ()
        (Tuple () TNil ())
        ()
        (((), Ident "x", (), Var () () LocalScope (Expl (ExtIdent $ Right "someList")), Nothing) NonEmpty.:| [])
        Nothing
        ()
    shouldSucceedFor "[(x,y) | x <- someList, y <- otherList]" $
      ArrayComp
        ()
        (Tuple () (TCons (Var () () LocalScope (Expl (ExtIdent $ Right "x")), Just ()) (Var () () LocalScope (Expl (ExtIdent $ Right "y")), Nothing) []) ())
        ()
        (((), Ident "x", (), Var () () LocalScope (Expl (ExtIdent $ Right "someList")), Just ()) NonEmpty.:| [((), Ident "y", (), Var () () LocalScope (Expl (ExtIdent $ Right "otherList")), Nothing)])
        Nothing
        ()
    shouldSucceedFor "[2*x | x <- someList, if x > 10]" $
      ArrayComp
        ()
        (Op (Lit () (LInt 2)) () () (10, LeftFix) LocalScope (Ident "*") (Var () () LocalScope (Expl (ExtIdent $ Right "x"))))
        ()
        (((), Ident "x", (), Var () () LocalScope (Expl (ExtIdent $ Right "someList")), Just ()) NonEmpty.:| [])
        (Just ((), Op (Var () () LocalScope (Expl (ExtIdent $ Right "x"))) () () (7, NoFix) LocalScope (Ident ">") (Lit () (LInt 10))))
        ()
    shouldFailFor "[() | if x > 10]"

  describe "parsing type annotations" $ do
    shouldFailFor "let d : forall 'b 'a2 . {requires numeric on double} ⇒ series of (array of 'a1) = None in 0"
  where
    shouldSucceedFor str ast =
      it ("should succeed for \"" <> unpack str <> "\"") $
        case parseExpr (baseOpsTable prelude) (builtinModulesOpsTable prelude) [] str of
          Left err -> expectationFailure $ "Failed with: " <> prettyError (fst $ NonEmpty.head err)
          Right (res, _) -> void res `shouldBe` ast
    shouldFailFor str =
      it ("should fail for \"" <> unpack str <> "\"") $
        case parseExpr (baseOpsTable prelude) (builtinModulesOpsTable prelude) [] str of
          Left _err -> pure ()
          Right _res -> expectationFailure "This should not parse"
