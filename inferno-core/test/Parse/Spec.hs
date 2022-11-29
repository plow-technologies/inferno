{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Parse.Spec where

import Data.Functor.Foldable (ana, project)
import qualified Data.IntMap as IntMap (elems, toList)
import qualified Data.List.NonEmpty as NEList
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.Lazy (toStrict)
-- import Inferno.Module.Prelude (baseOpsTable, builtinModulesOpsTable)
import Inferno.Parse (parseExpr, prettyError)
import Inferno.Types.Syntax
  ( BlockUtils (removeComments),
    Comment (..),
    Expr (..),
    ExtIdent (..),
    Fixity (..),
    IStr (..),
    Ident (Ident),
    ImplExpl (..),
    Import (..),
    InfixFixity (..),
    Lit (..),
    ModuleName (..),
    Pat (..),
    Scoped (..),
    SomeIStr (..),
    TList (..),
    arbitraryName,
    tListFromList,
  )
import Inferno.Utils.Prettyprinter (renderPretty)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    PrintableString (getPrintableString),
    Property,
    Testable (property),
    choose,
    counterexample,
    genericShrink,
    oneof,
    recursivelyShrink,
    shrinkNothing,
    sized,
    suchThat,
    (===),
  )
import Text.Pretty.Simple (pShow)
import Utils (baseOpsTable, builtinModulesOpsTable)

instance Arbitrary a => Arbitrary (Scoped a) where
  arbitrary = oneof $ [pure LocalScope, Scope <$> arbitrary]
  shrink = shrinkNothing

instance Arbitrary InfixFixity where
  arbitrary = oneof $ map pure [NoFix, LeftFix, RightFix]
  shrink = shrinkNothing

instance Arbitrary Lit where
  arbitrary =
    oneof
      [ LInt <$> arbitrary,
        LDouble <$> arbitrary,
        (LText . pack . getPrintableString) <$> arbitrary,
        LHex <$> arbitrary
      ]

instance Arbitrary ImplExpl where
  shrink = shrinkNothing
  arbitrary =
    oneof
      [ Impl <$> arbitrary,
        Expl <$> arbitrary
      ]

instance Arbitrary (Import ()) where
  shrink = shrinkNothing
  arbitrary =
    oneof
      [ IVar () <$> arbitrary,
        IOpVar () <$> arbitrary,
        IEnum () () <$> arbitrary
      ]

instance Arbitrary (Comment ()) where
  shrink = shrinkNothing
  arbitrary =
    oneof
      [ (\x -> LineComment () x ()) <$> (pack . getPrintableString <$> arbitrary) `suchThat` (Text.all $ \c -> c /= '\n' && c /= '\r'),
        (\x -> BlockComment () x ()) <$> (pack . getPrintableString <$> arbitrary) `suchThat` (Text.all $ \c -> c /= '*') -- prevent having a '*/'
      ]

instance Arbitrary a => Arbitrary (SomeIStr a) where
  arbitrary = sized $ \n -> do
    k <- choose (0, n)
    oneof [SomeIStr <$> goT k, SomeIStr <$> goF k]
    where
      goT :: Int -> Gen (IStr 'True a)
      goT = \case
        0 -> pure ISEmpty
        n -> oneof [ISExpr <$> arbitrary <*> goT (n -1), ISExpr <$> arbitrary <*> goF (n -1)]

      goF :: Int -> Gen (IStr 'False a)
      goF = \case
        0 -> ISStr <$> arbitrary <*> pure ISEmpty
        n -> ISStr <$> arbitrary <*> goT (n -1)

  shrink (SomeIStr ISEmpty) = []
  shrink (SomeIStr (ISStr s xs)) =
    -- shrink to subterms
    [SomeIStr xs]
      ++
      -- recursively shrink subterms
      [ case xs' of
          SomeIStr (ISStr _ _) -> xs'
          SomeIStr r@(ISExpr _ _) -> SomeIStr $ ISStr s r
          SomeIStr r@ISEmpty -> SomeIStr $ ISStr s r
        | xs' <- shrink (SomeIStr xs)
      ]
  shrink (SomeIStr (ISExpr e xs)) =
    [SomeIStr xs]
      ++ [SomeIStr (ISExpr e' xs) | e' <- shrink e]
      ++
      -- recursively shrink subterms
      [SomeIStr (ISExpr e' xs') | (e', SomeIStr xs') <- shrink (e, SomeIStr xs)]

instance Arbitrary (Pat () ()) where
  arbitrary = sized arbitrarySizedPat
  shrink = recursivelyShrink

arbitrarySizedPat :: Int -> Gen (Pat () ())
arbitrarySizedPat n =
  oneof
    [ pure $ PVar () Nothing,
      PVar () . Just <$> arbitrary,
      PEnum () () LocalScope <$> arbitrary,
      PLit () <$> arbitrary,
      POne () <$> arbitrarySizedPat n,
      pure $ PEmpty (),
      PCommentAbove <$> arbitrary <*> arbitrarySizedPat (n `div` 3),
      PCommentAfter <$> arbitrarySizedPat (n `div` 3) <*> arbitrary,
      PCommentBelow <$> arbitrarySizedPat (n `div` 3) <*> arbitrary,
      (\xs -> PTuple () (tListFromList xs) ()) <$> do
        k <- choose (0, n)
        sequence [(,Nothing) <$> arbitrarySizedPat (n `div` 3) | _ <- [1 .. k]]
        `suchThat` (\xs -> length xs /= 1)
    ]

instance Arbitrary e => Arbitrary (NEList.NonEmpty e) where
  arbitrary = NEList.fromList <$> (arbitrary `suchThat` (not . null))
  shrink = genericShrink

instance Arbitrary (Expr () ()) where
  shrink = recursivelyShrink
  arbitrary = sized arbitrarySized
    where
      -- Don't generate implicit variables, because parser does not support them
      arbitraryExtIdent = ExtIdent <$> Right <$> arbitraryName
      arbitraryImplExpl = oneof [Impl <$> arbitraryExtIdent, Expl <$> arbitraryExtIdent]
      arbitraryVar =
        oneof
          [ Var () () LocalScope <$> arbitraryImplExpl,
            OpVar () () LocalScope . Ident
              <$> ( oneof $
                      concatMap
                        ( \case
                            (InfixOp _, _, op) -> [pure op]
                            _ -> []
                        )
                        $ concat $ IntMap.elems baseOpsTable
                  )
          ]
      arbitraryEnum = Enum () () LocalScope <$> arbitrary
      arbitraryLit = Lit () <$> arbitrary

      arbitraryApp n =
        App
          <$> (arbitrarySized $ n `div` 3)
          <*> (arbitrarySized $ n `div` 3)

      arbitraryLam n =
        (\vs e -> Lam () vs () e)
          <$> arbitraryLamVars <*> (arbitrarySized $ n `div` 3)
        where
          -- Don't generate implicit vars. Sorry, there must be a nicer way to do this
          arbitraryLamVars :: Gen (NEList.NonEmpty ((), Maybe ExtIdent))
          arbitraryLamVars = arbitrary `suchThat` (all isSomeRight . snd . NEList.unzip)
          isSomeRight (Just (ExtIdent (Right _))) = True
          isSomeRight _ = False

      arbitraryLet n =
        (\v e1 e2 -> Let () () v () e1 () e2)
          <$> arbitraryImplExpl <*> (arbitrarySized $ n `div` 3) <*> (arbitrarySized $ n `div` 3)

      arbitraryIString n =
        (\xs -> InterpolatedString () xs ())
          <$> do
            k <- choose (0, n)
            oneof [SomeIStr <$> goT k, SomeIStr <$> goF k]
        where
          goT :: Int -> Gen (IStr 'True ((), Expr () (), ()))
          goT = \case
            0 -> pure ISEmpty
            m ->
              oneof
                [ ISExpr <$> ((\x -> ((), x, ())) <$> (arbitrarySized $ n `div` 3)) <*> goT (m -1),
                  ISExpr <$> ((\x -> ((), x, ())) <$> (arbitrarySized $ n `div` 3)) <*> goF (m -1)
                ]

          goF :: Int -> Gen (IStr 'False ((), Expr () (), ()))
          goF = \case
            0 ->
              ISStr
                <$> ( (pack . getPrintableString <$> arbitrary)
                        `suchThat` (\x -> not (Text.null x) && Text.all (\c -> c /= '\\' && c /= '$' && c /= '`') x)
                    )
                <*> pure ISEmpty
            m ->
              ISStr
                <$> ( (pack . getPrintableString <$> arbitrary)
                        `suchThat` (\x -> not (Text.null x) && Text.all (\c -> c /= '\\' && c /= '$' && c /= '`') x)
                    )
                <*> goT (m -1)

      arbitraryIf n =
        (\c t f -> If () c () t () f)
          <$> (arbitrarySized $ n `div` 3) <*> (arbitrarySized $ n `div` 3) <*> (arbitrarySized $ n `div` 3)

      arbitraryAssert n =
        (\c e -> Assert () c () e)
          <$> (arbitrarySized $ n `div` 3) <*> (arbitrarySized $ n `div` 3)

      arbitraryOp n =
        (\(prec, fix, op) e1 e2 -> Op e1 () () (prec, fix) LocalScope (Ident op) e2)
          <$> ( oneof $
                  map pure $
                    concatMap
                      ( \(prec, xs) ->
                          concatMap
                            ( \case
                                (InfixOp fix, _, op) -> [(prec, fix, op)]
                                _ -> []
                            )
                            xs
                      )
                      $ IntMap.toList baseOpsTable
              )
          <*> (arbitrarySized $ n `div` 3)
          <*> (arbitrarySized $ n `div` 3)

      arbitraryPreOp n =
        (\(prec, op) e -> PreOp () () prec LocalScope (Ident op) e)
          <$> ( oneof $
                  map pure $
                    concatMap
                      ( \(prec, xs) ->
                          concatMap
                            ( \case
                                (PrefixOp, _, op) -> [(prec, op)]
                                _ -> []
                            )
                            xs
                      )
                      $ IntMap.toList baseOpsTable
              )
          <*> (arbitrarySized $ n `div` 3)

      arbitraryCase n =
        (\e cs -> Case () e () (NEList.fromList cs) ())
          <$> (arbitrarySized $ n `div` 3)
          <*> do
            k <- choose (0, n)
            sequence
              [ (\i e -> ((), i, (), e))
                  <$> arbitrarySizedPat (n `div` 3) <*> arbitrarySized (n `div` 3)
                | _ <- [1 .. k]
              ]
            `suchThat` (not . null)

      arbitraryBracketed n = (\e -> Bracketed () e ()) <$> arbitrarySized (n `div` 3)
      arbitrarySized 0 =
        oneof
          [ arbitraryVar,
            arbitraryEnum,
            arbitraryLit,
            pure $ Empty ()
          ]
      arbitrarySized n =
        oneof
          [ arbitraryVar,
            arbitraryEnum,
            arbitraryLit,
            arbitraryApp n,
            arbitraryLam n,
            arbitraryLet n,
            arbitraryIString n,
            arbitraryIf n,
            arbitraryOp n,
            arbitraryPreOp n,
            (\xs -> Array () xs ())
              <$> ( do
                      k <- choose (0, n)
                      sequence [(,Nothing) <$> arbitrarySized (n `div` 3) | _ <- [1 .. k]]
                  ),
            One () <$> arbitrarySized (n `div` 3),
            pure $ Empty (),
            arbitraryAssert n,
            arbitraryCase n,
            (\e xs c -> ArrayComp () e () (NEList.fromList [((), x, (), e', Nothing) | (x, e') <- xs]) c ())
              <$> (arbitrarySized $ n `div` 3)
              <*> do
                k <- choose (0, n)
                sequence [(,) <$> arbitrary <*> arbitrarySized (n `div` 3) | _ <- [1 .. k]]
                `suchThat` (not . null)
              <*> oneof [Just . ((),) <$> (arbitrarySized $ n `div` 3), pure Nothing],
            arbitraryBracketed n,
            CommentAbove <$> arbitrary <*> arbitrarySized (n `div` 3),
            CommentAfter <$> arbitrarySized (n `div` 3) <*> arbitrary,
            CommentBelow <$> arbitrarySized (n `div` 3) <*> arbitrary
          ]

normalizePat :: Pat h a -> Pat h a
normalizePat = ana $ \case
  PTuple p1 xs p2 -> project $ PTuple p1 (fmap (\(e, _) -> (normalizePat e, Nothing)) xs) p2
  x -> project x

normalizeExpr :: Expr h a -> Expr h a
normalizeExpr = ana $ \case
  PreOp pos hsh prec LocalScope (Ident "-") e -> case normalizeExpr e of
    Lit l' (LInt x) -> project $ Lit l' $ LInt $ - x
    Lit l' (LDouble x) -> project $ Lit l' $ LDouble $ - x
    PreOp _ _ _ LocalScope (Ident "-") e' -> project $ e'
    e' -> project $ PreOp pos hsh prec LocalScope (Ident "-") e'
  Tuple p1 xs p2 -> project $ Tuple p1 (fmap (\(e, _) -> (normalizeExpr e, Nothing)) xs) p2
  Array p1 xs p2 -> project $ Array p1 (fmap (\(e, _) -> (normalizeExpr e, Nothing)) xs) p2
  ArrayComp p1 e_body p2 args e_cond p3 ->
    project $
      ArrayComp
        p1
        (normalizeExpr e_body)
        p2
        (fmap (\(p4, x, p5, e, _) -> (p4, x, p5, normalizeExpr e, Nothing)) args)
        (fmap (\(p4, e) -> (p4, normalizeExpr e)) e_cond)
        p3
  Bracketed _ e _ -> project $ normalizeExpr e
  Op e1 p1 h (_, fix) modNm i e2 -> project $ Op (normalizeExpr e1) p1 h (0, fix) modNm i (normalizeExpr e2)
  Case p1 e_case p2 patExprs p3 -> project $ Case p1 (normalizeExpr e_case) p2 (fmap (\(p4, p, p5, e) -> (p4, normalizePat p, p5, normalizeExpr e)) patExprs) p3
  x -> project x

(<?>) :: (Testable p) => p -> Text -> Property
(<?>) = flip (counterexample . unpack)

infixl 2 <?>

parsingTests :: Spec
parsingTests = describe "pretty printing/parsing" $ do
  prop "parseExpr and pretty are inverse up to normalizeExpr" $
    \(x :: Expr () ()) -> case parseExpr baseOpsTable builtinModulesOpsTable (renderPretty x) of
      Left err ->
        property False
          <?> ( "Pretty: \n" <> (renderPretty x)
                  <> "\nParse error:\n"
                  <> (pack $ prettyError $ fst $ NEList.head err)
              )
      Right (res, _comments) ->
        (normalizeExpr (removeComments x) === normalizeExpr (fmap (const ()) res))
          <?> ( "Pretty: \n" <> (renderPretty x)
                  <> "\nParsed: \n"
                  <> (toStrict $ pShow res)
                  <> "\nParsed pretty: \n"
                  <> (renderPretty res)
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
    shouldSucceedFor "``" $ InterpolatedString () (SomeIStr $ ISEmpty) ()
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
    shouldSucceedFor "fun _x -> ()" $ Lam () (((), Nothing) NEList.:| []) () (Tuple () TNil ())

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
    shouldSucceedFor "match () with { () -> ()}" $ Case () (Tuple () TNil ()) () (((), PTuple () TNil (), (), Tuple () TNil ()) NEList.:| []) ()
    shouldSucceedFor "match () with { | () -> ()}" $ Case () (Tuple () TNil ()) () (((), PTuple () TNil (), (), Tuple () TNil ()) NEList.:| []) ()

  describe "parsing assertions" $ do
    shouldFailFor "assert #false"
    shouldSucceedFor "assert #false in ()" $ Assert () (Enum () () LocalScope (Ident "false")) () (Tuple () TNil ())

  describe "parsing array builder" $ do
    shouldSucceedFor "[() | x <- someList]" $
      ArrayComp
        ()
        (Tuple () TNil ())
        ()
        (((), Ident "x", (), Var () () LocalScope (Expl (ExtIdent $ Right "someList")), Nothing) NEList.:| [])
        Nothing
        ()
    shouldSucceedFor "[(x,y) | x <- someList, y <- otherList]" $
      ArrayComp
        ()
        (Tuple () (TCons (Var () () LocalScope (Expl (ExtIdent $ Right "x")), Just ()) (Var () () LocalScope (Expl (ExtIdent $ Right "y")), Nothing) []) ())
        ()
        (((), Ident "x", (), Var () () LocalScope (Expl (ExtIdent $ Right "someList")), Just ()) NEList.:| [((), Ident "y", (), Var () () LocalScope (Expl (ExtIdent $ Right "otherList")), Nothing)])
        Nothing
        ()
    shouldSucceedFor "[2*x | x <- someList, if x > 10]" $
      ArrayComp
        ()
        (Op (Lit () (LInt 2)) () () (10, LeftFix) LocalScope (Ident "*") (Var () () LocalScope (Expl (ExtIdent $ Right "x"))))
        ()
        (((), Ident "x", (), Var () () LocalScope (Expl (ExtIdent $ Right "someList")), Just ()) NEList.:| [])
        (Just ((), Op (Var () () LocalScope (Expl (ExtIdent $ Right "x"))) () () (7, NoFix) LocalScope (Ident ">") (Lit () (LInt 10))))
        ()
    shouldFailFor "[() | if x > 10]"
  where
    shouldSucceedFor str ast =
      it ("should succeed for \"" <> unpack str <> "\"") $
        case parseExpr baseOpsTable builtinModulesOpsTable str of
          Left err -> expectationFailure $ "Failed with: " <> (prettyError $ fst $ NEList.head err)
          Right (res, _) -> fmap (const ()) res `shouldBe` ast
    shouldFailFor str =
      it ("should fail for \"" <> unpack str <> "\"") $
        case parseExpr baseOpsTable builtinModulesOpsTable str of
          Left _err -> pure ()
          Right _res -> expectationFailure $ "This should not parse"
