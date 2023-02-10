{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances have their own module and its part of inferno-core
--   given expr generator uses the Prelude so it can't be in Syntax.hs (inferno-types) because
--   of circular dependencies. And it can't be in test/Parse/Spec.hs because it's needed
--   for Arbitrary VCObject (inferno-vc).
--   On the other hand, is arguable that given all the tests right now are on inferno-core
--   we could move this module to test/ folder, but by providing the instances as part of the core, we allow others to re-use them.
module Inferno.Instances.Arbitrary where

import Control.Monad.Except (ExceptT)
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA256)
import qualified Data.ByteString as ByteString (pack)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.IntMap as IntMap (elems, toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set (fromList)
import qualified Data.Text as Text (Text, all, null, pack)
import GHC.Generics (Generic (..), Rep)
import Inferno.Eval.Error (EvalError)
import qualified Inferno.Module.Prelude as Prelude
import qualified Inferno.Types.Module as Module (Module)
import Inferno.Types.Syntax
  ( BaseType (..),
    Comment (..),
    Expr (..),
    ExtIdent (..),
    Fixity (..),
    IStr (..),
    Ident (Ident),
    ImplExpl (..),
    Import (..),
    InfernoType (..),
    InfixFixity,
    Lit (..),
    ModuleName (..),
    OpsTable,
    Pat (..),
    Scoped (..),
    SomeIStr (..),
    TList (..),
    TV (..),
    rws,
    tListFromList,
  )
import qualified Inferno.Types.Type as Type
  ( ImplType,
    Namespace (..),
    TCScheme,
    TypeClass,
  )
import qualified Inferno.Types.VersionControl as VersionControl
  ( Pinned (..),
    VCObjectHash (..),
  )
import qualified Inferno.VersionControl.Types as VersionControl
  ( VCIncompatReason (..),
    VCMeta (..),
    VCObject (..),
    VCObjectPred (..),
    VCObjectVisibility (..),
  )
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    PrintableString (getPrintableString),
    chooseInt,
    elements,
    listOf,
    oneof,
    recursivelyShrink,
    shrinkNothing,
    sized,
    suchThat,
  )
import Test.QuickCheck.Arbitrary.ADT (GArbitrary, ToADTArbitrary (..), genericArbitrary)
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Instances.Text ()

-- | A utility type for deriving Arbitrary for simple types
-- Use as @deriving Arbitrary via (GenericArbitrary MyType)@
newtype GenericArbitrary a = GenericArbitrary a

instance (Generic a, GArbitrary ga, ga ~ Rep a) => Arbitrary (GenericArbitrary a) where
  arbitrary = GenericArbitrary <$> genericArbitrary

baseOpsTable :: OpsTable
baseOpsTable = Prelude.baseOpsTable @(ExceptT EvalError IO) @() $ Prelude.builtinModules @(ExceptT EvalError IO) @()

-- | Arbitrary and ToADTArbitrary instances for Inferno.Types.Module
deriving instance Arbitrary objs => ToADTArbitrary (Module.Module objs)

deriving via (GenericArbitrary (Module.Module objs)) instance Arbitrary objs => Arbitrary (Module.Module objs)

-- | Arbitrary and ToADTArbitrary instances for Inferno.Types.Syntax
deriving instance ToADTArbitrary TV

deriving via (GenericArbitrary TV) instance Arbitrary TV

deriving instance ToADTArbitrary BaseType

instance Arbitrary BaseType where
  shrink = shrinkNothing
  arbitrary =
    oneof $
      (TEnum <$> (Text.pack <$> arbitrary) <*> (Set.fromList <$> listOf arbitrary))
        : ( map
              pure
              [ TInt,
                TDouble,
                TWord16,
                TWord32,
                TWord64,
                TText,
                TTime,
                TTimeDiff,
                TResolution
              ]
          )

deriving instance ToADTArbitrary InfernoType

instance Arbitrary InfernoType where
  shrink = recursivelyShrink
  arbitrary = sized arbitrarySized
    where
      arbitraryVar =
        TVar <$> arbitrary

      arbitraryArr n =
        TArr
          <$> (arbitrarySized $ n `div` 3)
          <*> (arbitrarySized $ n `div` 3)

      arbitraryTTuple n =
        oneof
          [ pure $ TTuple TNil,
            TTuple <$> (TCons <$> (arbitrarySized $ n `div` 3) <*> (arbitrarySized $ n `div` 3) <*> listOf (arbitrarySized $ n `div` 3))
          ]

      arbitraryBase = TBase <$> arbitrary

      arbitraryRest n = do
        constr <- elements [TArray, TSeries, TOptional, TRep]
        constr <$> (arbitrarySized $ n `div` 3)

      arbitrarySized 0 =
        oneof
          [ arbitraryVar,
            arbitraryBase
          ]
      arbitrarySized n =
        oneof
          [ arbitraryVar,
            arbitraryBase,
            arbitraryArr n,
            arbitraryTTuple n,
            arbitraryRest n
          ]

arbitraryName :: Gen Text.Text
arbitraryName =
  ( (\a as -> Text.pack $ a : as)
      <$> (elements ['a' .. 'z'])
      <*> (listOf $ elements $ ['0' .. '9'] ++ ['a' .. 'z'] ++ ['_'])
  )
    `suchThat` (\i -> not $ i `elem` rws)

deriving instance ToADTArbitrary Ident

instance Arbitrary Ident where
  shrink = shrinkNothing
  arbitrary = Ident <$> arbitraryName

deriving instance ToADTArbitrary ModuleName

instance Arbitrary ModuleName where
  shrink = shrinkNothing
  arbitrary = ModuleName <$> arbitraryName

deriving instance ToADTArbitrary ExtIdent

instance Arbitrary ExtIdent where
  shrink = shrinkNothing
  arbitrary =
    ExtIdent <$> oneof [Left <$> (arbitrary `suchThat` ((<) 0)), Right <$> arbitraryName]

deriving instance ToADTArbitrary ImplExpl

deriving via (GenericArbitrary ImplExpl) instance Arbitrary ImplExpl

deriving instance ToADTArbitrary InfixFixity

deriving via (GenericArbitrary InfixFixity) instance Arbitrary InfixFixity

deriving instance ToADTArbitrary Fixity

deriving via (GenericArbitrary Fixity) instance Arbitrary Fixity

instance Arbitrary pos => Arbitrary (Comment pos) where
  shrink = shrinkNothing
  arbitrary =
    oneof
      [ LineComment
          <$> arbitrary
          <*> (Text.pack . getPrintableString <$> arbitrary) `suchThat` (Text.all $ \c -> c /= '\n' && c /= '\r')
          <*> arbitrary,
        BlockComment
          <$> arbitrary
          <*> (Text.pack . getPrintableString <$> arbitrary) `suchThat` (Text.all $ \c -> c /= '*') -- prevent having a '*/'
          <*> arbitrary
      ]

deriving instance ToADTArbitrary Lit

instance Arbitrary Lit where
  arbitrary =
    oneof
      [ LInt <$> arbitrary,
        LDouble <$> arbitrary,
        (LText . Text.pack . getPrintableString) <$> arbitrary,
        LHex <$> arbitrary
      ]

deriving instance Arbitrary a => ToADTArbitrary (TList a)

instance Arbitrary a => Arbitrary (TList a) where
  arbitrary =
    oneof
      [ pure TNil,
        TCons <$> arbitrary <*> arbitrary <*> listOf arbitrary
      ]

instance Arbitrary a => Arbitrary (SomeIStr a) where
  arbitrary = sized $ \n -> do
    k <- chooseInt (0, n)
    oneof [SomeIStr <$> goT k, SomeIStr <$> goF k]
    where
      goT :: Arbitrary a => Int -> Gen (IStr 'True a)
      goT = \case
        0 -> pure ISEmpty
        n -> oneof [ISExpr <$> arbitrary <*> goT (n - 1), ISExpr <$> arbitrary <*> goF (n - 1)]

      goF :: Arbitrary a => Int -> Gen (IStr 'False a)
      goF = \case
        0 -> ISStr <$> arbitrary <*> pure ISEmpty
        n -> ISStr <$> arbitrary <*> goT (n - 1)

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

-- | TODO: I changed to the explicit instance below because it was causing the tests to hang.
--   Perhaps the best way would be to use a size bound on the recursive constructors like "ICommentAbove"
-- deriving instance Arbitrary pos => ToADTArbitrary (Import pos)
-- deriving via (GenericArbitrary (Import pos)) instance Arbitrary pos => Arbitrary (Import pos)
instance Arbitrary pos => Arbitrary (Import pos) where
  shrink = shrinkNothing
  arbitrary =
    oneof
      [ IVar <$> arbitrary <*> arbitrary,
        IOpVar <$> arbitrary <*> arbitrary,
        IEnum <$> arbitrary <*> arbitrary <*> arbitrary
      ]

deriving instance Arbitrary a => ToADTArbitrary (Scoped a)

deriving via (GenericArbitrary (Scoped a)) instance Arbitrary a => Arbitrary (Scoped a)

-- NOTE: this instance doesn't generate all Exprs, it only generates some valid ones
-- This is because the parser tests use this. However, golden tests in theory should
-- test all Exprs, so at some point we should revisit this and generate the missing ones
-- (e.g., this doesn't generate any Exprs with implicit vars)
-- TODO does inferno-vc store scripts with implicit vars?
instance (Arbitrary hash, Arbitrary pos) => Arbitrary (Expr hash pos) where
  shrink = recursivelyShrink
  arbitrary = sized arbitrarySized
    where
      -- Don't generate implicit variables, because parser does not support them
      arbitraryExtIdent = ExtIdent <$> Right <$> arbitraryName
      arbitraryImplExpl = oneof [Impl <$> arbitraryExtIdent, Expl <$> arbitraryExtIdent]
      arbitraryVar :: (Arbitrary hash, Arbitrary pos) => Gen (Expr hash pos)
      arbitraryVar =
        oneof
          [ Var <$> arbitrary <*> arbitrary <*> pure LocalScope <*> arbitraryImplExpl,
            OpVar
              <$> arbitrary
              <*> arbitrary
              <*> pure LocalScope
              <*> ( Ident
                      <$> ( oneof
                              $ concatMap
                                ( \case
                                    (InfixOp _, _, op) -> [pure op]
                                    _ -> []
                                )
                              $ concat
                              $ IntMap.elems baseOpsTable
                          )
                  )
          ]
      arbitraryEnum :: (Arbitrary hash, Arbitrary pos) => Gen (Expr hash pos)
      arbitraryEnum = Enum <$> arbitrary <*> arbitrary <*> pure LocalScope <*> arbitrary
      arbitraryLit :: (Arbitrary hash, Arbitrary pos) => Gen (Expr hash pos)
      arbitraryLit = Lit <$> arbitrary <*> arbitrary

      arbitraryApp n =
        App
          <$> (arbitrarySized $ n `div` 3)
          <*> (arbitrarySized $ n `div` 3)

      arbitraryLam n =
        Lam
          <$> arbitrary
          <*> arbitraryLamVars
          <*> arbitrary
          <*> (arbitrarySized $ n `div` 3)
        where
          -- Don't generate implicit vars. Sorry, there must be a nicer way to do this
          arbitraryLamVars :: Arbitrary pos => Gen (NonEmpty (pos, Maybe ExtIdent))
          arbitraryLamVars = arbitrary `suchThat` (all isSomeRight . snd . NonEmpty.unzip)
          isSomeRight (Just (ExtIdent (Right _))) = True
          isSomeRight _ = False

      arbitraryLet n =
        Let
          <$> arbitrary
          <*> arbitrary
          <*> arbitraryImplExpl
          <*> arbitrary
          <*> (arbitrarySized $ n `div` 3)
          <*> arbitrary
          <*> (arbitrarySized $ n `div` 3)

      arbitraryIString n =
        InterpolatedString
          <$> arbitrary
          <*> do
            k <- chooseInt (0, n)
            oneof [SomeIStr <$> goT k, SomeIStr <$> goF k]
          <*> arbitrary
        where
          goT :: (Arbitrary hash, Arbitrary pos) => Int -> Gen (IStr 'True (pos, Expr hash pos, pos))
          goT = \case
            0 -> pure ISEmpty
            m ->
              oneof
                [ ISExpr <$> ((,,) <$> arbitrary <*> (arbitrarySized $ n `div` 3) <*> arbitrary) <*> goT (m - 1),
                  ISExpr <$> ((,,) <$> arbitrary <*> (arbitrarySized $ n `div` 3) <*> arbitrary) <*> goF (m - 1)
                ]

          goF :: (Arbitrary hash, Arbitrary pos) => Int -> Gen (IStr 'False (pos, Expr hash pos, pos))
          goF = \case
            0 ->
              ISStr
                <$> ( (Text.pack . getPrintableString <$> arbitrary)
                        `suchThat` (\x -> not (Text.null x) && Text.all (\c -> c /= '\\' && c /= '$' && c /= '`') x)
                    )
                <*> pure ISEmpty
            m ->
              ISStr
                <$> ( (Text.pack . getPrintableString <$> arbitrary)
                        `suchThat` (\x -> not (Text.null x) && Text.all (\c -> c /= '\\' && c /= '$' && c /= '`') x)
                    )
                <*> goT (m - 1)

      arbitraryIf n =
        If
          <$> arbitrary
          <*> (arbitrarySized $ n `div` 3)
          <*> arbitrary
          <*> (arbitrarySized $ n `div` 3)
          <*> arbitrary
          <*> (arbitrarySized $ n `div` 3)

      arbitraryAssert n =
        Assert
          <$> arbitrary
          <*> (arbitrarySized $ n `div` 3)
          <*> arbitrary
          <*> (arbitrarySized $ n `div` 3)

      arbitraryOp n =
        (\(prec, fix, op) e1 e2 p h -> Op e1 p h (prec, fix) LocalScope (Ident op) e2)
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
          <*> arbitrary
          <*> arbitrary

      arbitraryPreOp n =
        (\(prec, op) e p h -> PreOp p h prec LocalScope (Ident op) e)
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
          <*> arbitrary
          <*> arbitrary

      -- Only generate case statements of size 1 or above:
      arbitraryCase 0 = undefined
      arbitraryCase n =
        (\e cs p1 p2 p3 -> Case p1 e p2 (NonEmpty.fromList cs) p3)
          <$> (arbitrarySized $ n `div` 3)
          <*> ( do
                  k <- chooseInt (0, n)
                  sequence
                    [ (\i e p1 p2 -> (p1, i, p2, e))
                        <$> arbitrarySizedPat (n `div` (3 * k))
                        <*> arbitrarySized (n `div` (3 * k))
                        <*> arbitrary
                        <*> arbitrary
                      | _ <- [1 .. k]
                    ]
              )
            `suchThat` (not . null)
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary

      arbitraryBracketed n =
        Bracketed <$> arbitrary <*> arbitrarySized (n `div` 3) <*> arbitrary

      arbitraryArrayComp n =
        ArrayComp
          <$> arbitrary
          <*> (arbitrarySized $ n `div` 3)
          <*> arbitrary
          <*> ( NonEmpty.fromList
                  <$> do
                    k <- chooseInt (0, n)
                    sequence [(,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrarySized (n `div` 3) <*> pure Nothing | _ <- [1 .. k]]
                    `suchThat` (not . null)
              )
          <*> oneof [(\p e -> Just (p, e)) <$> arbitrary <*> (arbitrarySized $ n `div` 3), pure Nothing]
          <*> arbitrary

      arbitrarySized :: (Arbitrary hash, Arbitrary pos) => Int -> Gen (Expr hash pos)
      arbitrarySized 0 =
        oneof
          [ arbitraryVar,
            arbitraryEnum,
            arbitraryLit,
            Empty <$> arbitrary
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
            Array
              <$> arbitrary
              <*> ( do
                      k <- chooseInt (0, n)
                      sequence [(,Nothing) <$> arbitrarySized (n `div` 3) | _ <- [1 .. k]]
                  )
              <*> arbitrary,
            arbitraryArrayComp n,
            One <$> arbitrary <*> arbitrarySized (n `div` 3),
            Empty <$> arbitrary,
            arbitraryAssert n,
            arbitraryCase n,
            One <$> arbitrary <*> arbitrarySized (n `div` 3),
            arbitraryBracketed n,
            CommentAbove <$> arbitrary <*> arbitrarySized (n `div` 3),
            CommentAfter <$> arbitrarySized (n `div` 3) <*> arbitrary,
            CommentBelow <$> arbitrarySized (n `div` 3) <*> arbitrary
          ]

instance (Arbitrary hash, Arbitrary pos) => Arbitrary (Pat hash pos) where
  shrink = recursivelyShrink
  arbitrary = sized arbitrarySizedPat

arbitrarySizedPat :: (Arbitrary hash, Arbitrary pos) => Int -> Gen (Pat hash pos)
arbitrarySizedPat n =
  oneof
    [ PVar <$> arbitrary <*> pure Nothing,
      PVar <$> arbitrary <*> (Just <$> arbitrary),
      PEnum <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
      PLit <$> arbitrary <*> arbitrary,
      POne <$> arbitrary <*> arbitrarySizedPat (n `div` 3),
      PEmpty <$> arbitrary,
      PCommentAbove <$> arbitrary <*> arbitrarySizedPat (n `div` 3),
      PCommentAfter <$> arbitrarySizedPat (n `div` 3) <*> arbitrary,
      PCommentBelow <$> arbitrarySizedPat (n `div` 3) <*> arbitrary,
      (\p1 ts p2 -> PTuple p1 (tListFromList ts) p2)
        <$> arbitrary
        <*> ( do
                k <- chooseInt (0, n)
                sequence [(,Nothing) <$> arbitrarySizedPat (n `div` 3) | _ <- [1 .. k]]
            )
          `suchThat` (\xs -> length xs /= 1)
        <*> arbitrary
    ]

-- | Arbitrary and ToADTArbitrary instances for Inferno.Types.Type
deriving instance ToADTArbitrary Type.ImplType

deriving via (GenericArbitrary Type.ImplType) instance Arbitrary Type.ImplType

deriving instance ToADTArbitrary Type.TypeClass

deriving via (GenericArbitrary Type.TypeClass) instance Arbitrary Type.TypeClass

deriving instance ToADTArbitrary Type.TCScheme

deriving via (GenericArbitrary Type.TCScheme) instance Arbitrary Type.TCScheme

deriving instance ToADTArbitrary Type.Namespace

instance Arbitrary Type.Namespace where
  arbitrary =
    oneof
      [ Type.FunNamespace <$> arbitrary,
        Type.OpNamespace <$> arbitrary,
        Type.EnumNamespace <$> arbitrary,
        Type.ModuleNamespace <$> arbitrary,
        Type.TypeNamespace <$> arbitrary
      ]

-- | Arbitrary and ToADTArbitrary instances for Inferno.Types.VersionControl
instance Arbitrary (Digest SHA256) where
  arbitrary = hash . ByteString.pack <$> arbitrary

deriving instance ToADTArbitrary VersionControl.VCObjectHash

instance Arbitrary VersionControl.VCObjectHash where
  arbitrary = VersionControl.VCObjectHash . hash . Char8.pack <$> arbitrary

deriving instance Arbitrary a => ToADTArbitrary (VersionControl.Pinned a)

deriving via (GenericArbitrary (VersionControl.Pinned a)) instance Arbitrary a => Arbitrary (VersionControl.Pinned a)

-- | Arbitrary and ToADTArbitrary instances for Inferno.VersionControl.Types
deriving instance ToADTArbitrary VersionControl.VCObject

deriving via (GenericArbitrary VersionControl.VCObject) instance Arbitrary VersionControl.VCObject

deriving instance ToADTArbitrary VersionControl.VCObjectVisibility

instance Arbitrary VersionControl.VCObjectVisibility where
  arbitrary = oneof $ map pure [VersionControl.VCObjectPublic, VersionControl.VCObjectPrivate]

deriving instance ToADTArbitrary VersionControl.VCIncompatReason

instance Arbitrary VersionControl.VCIncompatReason where
  arbitrary = oneof $ map pure [VersionControl.TypeSignatureChange, VersionControl.EnumConstructorsChanged]

instance Arbitrary VersionControl.VCObjectPred where
  arbitrary =
    oneof
      [ pure VersionControl.Init,
        VersionControl.CompatibleWithPred <$> arbitrary,
        VersionControl.IncompatibleWithPred <$> arbitrary <*> arbitrary,
        VersionControl.MarkedBreakingWithPred <$> arbitrary,
        VersionControl.CloneOf <$> arbitrary
      ]

deriving instance ToADTArbitrary VersionControl.VCObjectPred

instance (Arbitrary a, Arbitrary g, Arbitrary o) => Arbitrary (VersionControl.VCMeta a g o) where
  arbitrary =
    VersionControl.VCMeta
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

deriving instance (Arbitrary a, Arbitrary g, Arbitrary o) => ToADTArbitrary (VersionControl.VCMeta a g o)
