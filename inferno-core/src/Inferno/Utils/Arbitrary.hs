{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- TODO write docstring explaining why this module is necessary
-- because the expr generator uses the Prelude, so it can't be in Syntax.hs because
-- of circular dependencies. And it can't be in test/Parse/Spec.hs because it's needed
-- for Arbitrary VCObject

module Inferno.Utils.Arbitrary where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.IntMap as IntMap (elems, toList)
import Data.Text (pack)
import qualified Data.Text as Text
import qualified Inferno.Module.Prelude as Prelude
import Inferno.Types.Syntax
  ( Expr (..),
    ExtIdent (..),
    Fixity (..),
    IStr (..),
    Ident (Ident),
    ImplExpl (..),
    Scoped (..),
    SomeIStr (..),
    arbitraryName, OpsTable, arbitrarySizedPat
  )
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    PrintableString (getPrintableString),
    choose,
    oneof,
    recursivelyShrink,
    sized,
    suchThat,
  )
import Control.Monad.Except (ExceptT)
import Inferno.Eval.Error (EvalError)
-- import Test.QuickCheck.Instances.Semigroup ()

baseOpsTable :: OpsTable
baseOpsTable = Prelude.baseOpsTable @(ExceptT EvalError IO) @() $ Prelude.builtinModules @(ExceptT EvalError IO) @()

-- NOTE: this instance doesn't generate all Exprs, it only generates some valid ones
-- This is because the parser tests use this. However, golden tests in theory should
-- test all Exprs, so at some point we should revisit this and generate the missing ones
-- (e.g., this doesn't generate any Exprs with implicit vars)

instance (Arbitrary hash, Arbitrary pos) => Arbitrary (Expr hash pos) where
  shrink = recursivelyShrink
  arbitrary = sized arbitrarySized
    where
      -- TODO does inferno-vc store scripts with implicit vars?
      -- Don't generate implicit variables, because parser does not support them
      arbitraryExtIdent = ExtIdent <$> Right <$> arbitraryName
      arbitraryImplExpl = oneof [Impl <$> arbitraryExtIdent, Expl <$> arbitraryExtIdent]
      arbitraryVar :: (Arbitrary hash, Arbitrary pos) => Gen (Expr hash pos)
      arbitraryVar =
        oneof
          [ Var <$> arbitrary <*> arbitrary <*> pure LocalScope <*> arbitraryImplExpl,
            OpVar <$> arbitrary <*> arbitrary <*> pure LocalScope <*> (Ident
              <$> ( oneof
                      $ concatMap
                        ( \case
                            (InfixOp _, _, op) -> [pure op]
                            _ -> []
                        )
                      $ concat
                      $ IntMap.elems baseOpsTable
                  ))
          ]
      arbitraryEnum :: (Arbitrary hash, Arbitrary pos) => Gen (Expr hash pos)
      arbitraryEnum = Enum  <$> arbitrary <*> arbitrary <*> pure LocalScope <*> arbitrary
      arbitraryLit :: (Arbitrary hash, Arbitrary pos) => Gen (Expr hash pos)
      arbitraryLit = Lit <$> arbitrary <*> arbitrary

      arbitraryApp n =
        App
          <$> (arbitrarySized $ n `div` 3)
          <*> (arbitrarySized $ n `div` 3)

      arbitraryLam n =
        (\vs e -> Lam () vs () e)
          <$> arbitraryLamVars
          <*> (arbitrarySized $ n `div` 3)
        where
          -- Don't generate implicit vars. Sorry, there must be a nicer way to do this
          arbitraryLamVars :: Gen (NonEmpty ((), Maybe ExtIdent))
          arbitraryLamVars = arbitrary `suchThat` (all isSomeRight . snd . NonEmpty.unzip)
          isSomeRight (Just (ExtIdent (Right _))) = True
          isSomeRight _ = False

      arbitraryLet n =
        (\v e1 e2 -> Let () () v () e1 () e2)
          <$> arbitraryImplExpl
          <*> (arbitrarySized $ n `div` 3)
          <*> (arbitrarySized $ n `div` 3)

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
                [ ISExpr <$> ((\x -> ((), x, ())) <$> (arbitrarySized $ n `div` 3)) <*> goT (m - 1),
                  ISExpr <$> ((\x -> ((), x, ())) <$> (arbitrarySized $ n `div` 3)) <*> goF (m - 1)
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
                <*> goT (m - 1)

      arbitraryIf n =
        (\c t f -> If () c () t () f)
          <$> (arbitrarySized $ n `div` 3)
          <*> (arbitrarySized $ n `div` 3)
          <*> (arbitrarySized $ n `div` 3)

      arbitraryAssert n =
        (\c e -> Assert () c () e)
          <$> (arbitrarySized $ n `div` 3)
          <*> (arbitrarySized $ n `div` 3)

      arbitraryOp n =
        (\(prec, fix, op) e1 e2 -> Op e1 () () (prec, fix) LocalScope (Ident op) e2)
          <$> ( oneof
                  $ map pure
                  $ concatMap
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
          <$> ( oneof
                  $ map pure
                  $ concatMap
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
        (\e cs -> Case () e () (NonEmpty.fromList cs) ())
          <$> (arbitrarySized $ n `div` 3)
          <*> do
            k <- choose (0, n)
            sequence
              [ (\i e -> ((), i, (), e))
                  <$> arbitrarySizedPat (n `div` 3)
                  <*> arbitrarySized (n `div` 3)
                | _ <- [1 .. k]
              ]
            `suchThat` (not . null)

      arbitraryBracketed n = (\e -> Bracketed () e ()) <$> arbitrarySized (n `div` 3)
      arbitrarySized :: (Arbitrary hash, Arbitrary pos) => Int -> Gen (Expr hash pos)
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
            (\e xs c -> ArrayComp () e () (NonEmpty.fromList [((), x, (), e', Nothing) | (x, e') <- xs]) c ())
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
