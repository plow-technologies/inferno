{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Inferno.Infer
  ( Constraint (UnifyConstraint, ClassConstraint),
    Subst (Subst),
    inferExpr,
    closeOver,
    closeOverType,
    findTypeClassWitnesses,
    inferTypeReps,
    inferPossibleTypes,
    unifyRecords,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, join, void, when, (<=<))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Extra (whenM, zipWithM_)
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (lift)
import Data.Bifunctor (bimap)
import Data.Foldable (foldl', for_, toList) -- foldl': see CLAUDE.md note about lower GHC versions
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Sequence (Seq, (<|))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as Vector.Mutable
import GHC.Records (HasField (getField))
import Inferno.Infer.Env
  ( Env,
    Namespace (EnumNamespace, OpNamespace),
    TypeMetadata (TypeMetadata, docs, identExpr, ty),
    closeOver,
    closeOverType,
  )
import qualified Inferno.Infer.Env as Env
import Inferno.Infer.Error
  ( TypeError
      ( AnnotationUnificationFail,
        AssertConditionMustBeBool,
        CaseBranchesMustBeEqType,
        DuplicateRecordField,
        ExpectedFunction,
        IfBranchesMustBeEqType,
        IfConditionMustBeBool,
        ImplicitVarTypeOverlap,
        InfiniteType,
        ModuleDoesNotExist,
        ModuleNameTaken,
        PatternUnificationFail,
        PatternsMustBeEqType,
        UnboundExtIdent,
        UnboundNameInNamespace,
        UnificationFail,
        VarMultipleOccurrence
      ),
  )
import Inferno.Module.Builtin (emptyHash, oneHash)
import Inferno.Types.Module (Module, PinnedModule)
import Inferno.Types.Syntax
  ( BlockUtils (blockPosition, removeComments),
    Comment,
    ElementPosition (elementPosition),
    Expr
      ( App,
        Array,
        ArrayComp,
        Assert,
        Bracketed,
        Case,
        CommentAbove,
        CommentAfter,
        CommentBelow,
        Empty,
        Enum,
        If,
        InterpolatedString,
        Lam,
        Let,
        LetAnnot,
        Lit,
        One,
        Op,
        OpVar,
        OpenModule,
        PreOp,
        Record,
        RecordField,
        RenameModule,
        Tuple,
        TypeRep,
        Var
      ),
    ExtIdent (ExtIdent),
    Ident (Ident),
    ImplExpl (Expl, Impl),
    Import,
    InfixFixity,
    Lit (LDouble, LHex, LInt, LText),
    ModuleName (ModuleName),
    Pat
      ( PArray,
        PCommentAbove,
        PCommentAfter,
        PCommentBelow,
        PEmpty,
        PEnum,
        PLit,
        POne,
        PRecord,
        PTuple,
        PVar
      ),
    RestOfRecord (RowAbsent, RowVar),
    Scoped (LocalScope),
    SomeIStr,
    TList,
    fromEitherList,
    fromScoped,
    incSourceCol,
    patternToExpr,
    tListFromList,
    tListToList,
    toEitherList,
  )
import Inferno.Types.Type
  ( ImplType (ImplType),
    InfernoType
      ( TArr,
        TArray,
        TBase,
        TOptional,
        TRecord,
        TRep,
        TSeries,
        TTuple,
        TVar
      ),
    Subst (Subst),
    Substitutable (apply, ftv),
    TCScheme (ForallTC),
    TV (TV, unTV),
    TypeClass (TypeClass),
    typeBool,
    typeDouble,
    typeInt,
    typeText,
    typeWord64,
  )
import Inferno.Types.VersionControl (Pinned (Local), VCObjectHash, pinnedToMaybe)
import Prettyprinter (Pretty (pretty), (<+>))
import Text.Megaparsec (SourcePos)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type Location a = (a, a)

-- | A constraint emitted during inference.
data Constraint
  = -- | Two types that must be equal, with error context if unification fails.
    UnifyConstraint !InfernoType !InfernoType ![TypeError SourcePos]
  | -- | A typeclass obligation at a source location.
    ClassConstraint !(Location SourcePos) !TypeClass

-- | Result of inferring a sub-expression: the elaborated expression,
-- its implicit type, and the generated typeclasses (for error context).
data InferResult = InferResult
  { expr :: Expr (Pinned VCObjectHash) SourcePos
  , typ :: ImplType
  , tcs :: !(Set TypeClass)
  }

-- | A single branch of a @match@ expression.
data CaseBranch = CaseBranch
  { barPos :: SourcePos
  , pat :: Pat (Pinned VCObjectHash) SourcePos
  , arrPos :: SourcePos
  , body :: Expr (Pinned VCObjectHash) SourcePos
  }

-- | Result of 'inferCase'. Contains the inferred scrutinee, the
-- branches with updated body expressions, and the overall type.
-- The caller ('infer') is responsible for assembling the 'Case' 'Expr'.
data CaseResult = CaseResult
  { scrutExpr :: Expr (Pinned VCObjectHash) SourcePos
  , branches :: NonEmpty CaseBranch
  , typ :: ImplType
  , tcs :: !(Set TypeClass)
  }

-- | A single selector in an array comprehension (@x <- gen@).
-- The @infer@ dispatcher constructs these from the raw 5-tuple in @ArrayComp@.
data Selector = Selector
  { identPos :: !SourcePos
  , ident :: !Ident
  , arrowPos :: !SourcePos
  , gen :: !(Expr (Pinned VCObjectHash) SourcePos)
  , commaPos :: !(Maybe SourcePos)
  }

-- | Result of processing all selectors, body, and condition in an
-- array comprehension. Built during the @processSels@ unwinding.
data CompResult = CompResult
  { rebuiltSels :: !(Seq Selector)
  , selImpls :: ![Map ExtIdent InfernoType]
  , selTcs :: !(Set TypeClass)
  , bodyResult :: !InferResult
  , condExpr :: !(Maybe (SourcePos, Expr (Pinned VCObjectHash) SourcePos))
  , condImpl :: !(Map ExtIdent InfernoType)
  , condTcs :: !(Set TypeClass)
  }

-- | Components of a @let@ binding (both explicit and implicit).
data LetBinding = LetBinding
  { letPos :: !SourcePos
  , varPos :: !SourcePos
  , ident :: !ExtIdent
  , eqPos :: !SourcePos
  , rhs :: !(Expr (Pinned VCObjectHash) SourcePos)
  , inPos :: !SourcePos
  , inExpr :: !(Expr (Pinned VCObjectHash) SourcePos)
  }

-- | Components of a type-annotated @let@ binding.
data LetAnnotBinding = LetAnnotBinding
  { letPos :: !SourcePos
  , varPos :: !SourcePos
  , ident :: !ExtIdent
  , annotPos :: !SourcePos
  , scheme :: !TCScheme
  , eqPos :: !SourcePos
  , rhs :: !(Expr (Pinned VCObjectHash) SourcePos)
  , inPos :: !SourcePos
  , inExpr :: !(Expr (Pinned VCObjectHash) SourcePos)
  }

-- | Components of an infix operator application @e1 `op` e2@.
data OpBinding = OpBinding
  { lhs :: !(Expr (Pinned VCObjectHash) SourcePos)
  , opPos :: !SourcePos
  , pin :: !(Pinned VCObjectHash)
  , opMeta :: !(Int, InfixFixity)
  , modNm :: !(Scoped ModuleName)
  , op :: !Ident
  , rhs :: !(Expr (Pinned VCObjectHash) SourcePos)
  }

-- | Components of a prefix operator application @op e@.
data PreOpBinding = PreOpBinding
  { opPos :: !SourcePos
  , pin :: !(Pinned VCObjectHash)
  , opLvl :: !Int
  , modNm :: !(Scoped ModuleName)
  , op :: !Ident
  , operand :: !(Expr (Pinned VCObjectHash) SourcePos)
  }

-- | Components of an @if cond then tr else fl@ expression.
data IfBinding = IfBinding
  { ifPos :: !SourcePos
  , cond :: !(Expr (Pinned VCObjectHash) SourcePos)
  , thenPos :: !SourcePos
  , tr :: !(Expr (Pinned VCObjectHash) SourcePos)
  , elsePos :: !SourcePos
  , fl :: !(Expr (Pinned VCObjectHash) SourcePos)
  }

-- | Components of an array comprehension @[body | sels if cond]@.
data ArrayCompBinding = ArrayCompBinding
  { open :: !SourcePos
  , body :: !(Expr (Pinned VCObjectHash) SourcePos)
  , pipe :: !SourcePos
  , sels :: !(NonEmpty Selector)
  , cond :: !(Maybe (SourcePos, Expr (Pinned VCObjectHash) SourcePos))
  , close :: !SourcePos
  }

-- | Components of a @let module NewName = OldName in e@ expression.
data RenameModuleBinding = RenameModuleBinding
  { namePos :: !SourcePos
  , newNm :: !ModuleName
  , oldPos :: !SourcePos
  , oldNm :: !ModuleName
  , inPos :: !SourcePos
  , body :: !(Expr (Pinned VCObjectHash) SourcePos)
  }

-- | Components of an @open Module (imports) in e@ expression.
data OpenModuleBinding = OpenModuleBinding
  { openPos :: !SourcePos
  , pin :: !(Pinned VCObjectHash)
  , modNm :: !ModuleName
  , imports :: ![(Import SourcePos, Maybe SourcePos)]
  , inPos :: !SourcePos
  , body :: !(Expr (Pinned VCObjectHash) SourcePos)
  }

-- | One side of a record-field merge: remaining sorted fields,
-- the row variable tail, and accumulated new fields.
data RecordSide = RecordSide
  { fields :: ![(Ident, InfernoType)]
  , ror :: !RestOfRecord
  , new :: ![(Ident, InfernoType)]
  }

-- | Virtual record fields for @ImplType@
instance HasField "impl" ImplType (Map ExtIdent InfernoType) where
  getField (ImplType m _) = m

instance HasField "body" ImplType InfernoType where
  getField (ImplType _ t) = t

instance Substitutable Constraint where
  apply s = \case
    UnifyConstraint t1 t2 es -> UnifyConstraint (apply s t1) (apply s t2) es
    ClassConstraint loc tc -> ClassConstraint loc $ apply s tc
  ftv = \case
    UnifyConstraint t1 t2 _ -> ftv t1 <> ftv t2
    ClassConstraint _ tc -> ftv tc

instance Pretty Constraint where
  pretty = \case
    UnifyConstraint t1 t2 _ -> pretty t1 <+> "~" <+> pretty t2
    ClassConstraint _ tc -> pretty tc

-------------------------------------------------------------------------------
-- Union-Find Cell Representation
-------------------------------------------------------------------------------

-- | A single cell in the union-find store.
-- @UFRoot@ is the root of a set; it carries a rank (for union-by-rank)
-- and an optional resolved (non-@TVar@) type.
-- @UFLink@ redirects to another type variable.
data UFContent
  = UFRoot !Int !(Maybe InfernoType)
  | UFLink !TV

-------------------------------------------------------------------------------
-- Monad Stack
-------------------------------------------------------------------------------

-- | Mutable union-find store, indexed by @unTV :: Int@.
-- Backed by a growable @MVector@ in @ST@.
type UFStore s = STRef s (STVector s UFContent)

-- | The inference monad: @ReaderT@ over @ExceptT@ over @ST@.
-- All mutable state lives in @STRef@s.
type Infer s a =
  ReaderT
    (InferCtx s)
    (ExceptT [TypeError SourcePos] (ST s))
    a

-- | Scoped (via @local@) and mutable (shared @STRef@s) context.
data InferCtx s = InferCtx
  { env :: !Env
  , refs :: !(InferRefs s)
  }

-- | All mutable references shared across the entire inference run.
data InferRefs s = InferRefs
  { count :: !(STRef s Int)
  , ufStore :: !(UFStore s)
  , typeMap ::
      !( STRef
          s
          (Map (Location SourcePos) (TypeMetadata (Set TypeClass, ImplType)))
       )
  , modules :: !(STRef s (Map ModuleName (Module ())))
  , tyClasses :: !(Set TypeClass)
  , patternsToCheck ::
      !( STRef
          s
          [(Location SourcePos, [Pat (Pinned VCObjectHash) SourcePos])]
       )
  , deferred :: !(STRef s [(Location SourcePos, TypeClass)])
  }

-- | Lift a raw @ST@ action into @Infer@.
liftST :: ST s a -> Infer s a
liftST = lift . lift

-- | Run an @ST@ action with the current UF store vector.
withStore :: (STVector s UFContent -> ST s a) -> Infer s a
withStore f = liftST . (f <=< readSTRef) =<< asks (.refs.ufStore)

-------------------------------------------------------------------------------
-- Union-Find Primitives
-------------------------------------------------------------------------------

-- | Ensure the UF store has capacity for index @i@.
-- Doubles the vector when needed.
ensureCapacity :: Int -> Infer s ()
ensureCapacity i =
  asks (.refs.ufStore) >>= \storeRef ->
    liftST $
      readSTRef storeRef >>= \v ->
        let cap0, cap1 :: Int
            cap0 = Vector.Mutable.length v
            cap1 = max (cap0 * 2) (i + 1)
         in when (i >= cap0) $
              writeSTRef storeRef =<< Vector.Mutable.grow v (cap1 - cap0)

-- | Allocate a fresh type variable, returning it as a 'TV'.
freshTV :: Infer s TV
freshTV = TV <$> freshTVRaw

-- | Allocate a fresh type variable, returning the raw @Int@ index.
freshTVRaw :: Infer s Int
freshTVRaw = do
  cRef <- asks (.refs.count)
  n <- liftST $ readSTRef cRef
  liftST . writeSTRef cRef $ n + 1
  ensureCapacity n
  withStore $ \v -> Vector.Mutable.write v n $ UFRoot 0 Nothing
  pure n

-- | Chase 'UFLink' pointers to the root, applying path compression.
ufFind :: TV -> Infer s TV
ufFind tv@(TV i) =
  withStore (`Vector.Mutable.read` i) >>= \case
    UFRoot _ _ -> pure tv
    UFLink parent -> do
      root <- ufFind parent
      -- Path compression
      when (root /= parent) $ withStore $ \v -> Vector.Mutable.write v i $ UFLink root
      pure root

-- | 'ufFind' in CPS style.
withFind :: TV -> (TV -> Infer s a) -> Infer s a
withFind = (>>=) . ufFind

-- | Find the root of @tv@ and return the type it represents.
-- If the root has a resolved type, return that; otherwise @TVar root@.
ufProbe :: TV -> Infer s InfernoType
ufProbe tv = withFind tv $ \root ->
  readRoot root >>= \case
    (_, Just t) -> pure t
    (_, Nothing) -> pure $ TVar root

-- | Read the rank and resolved type of a root cell.
-- Precondition: @tv@ must be a root (i.e. returned by 'ufFind').
readRoot :: TV -> Infer s (Int, Maybe InfernoType)
readRoot (TV ri) =
  withStore (`Vector.Mutable.read` ri) >>= \case
    UFRoot rank mt -> pure (rank, mt)
    -- NOTE: unreachable after ufFind
    UFLink _ -> pure (0, Nothing)

-- | Union two type variables by rank. If either root already has a
-- resolved type, the other root inherits it. If both have resolved
-- types, they are unified for consistency.
ufUnion :: TV -> TV -> Infer s ()
ufUnion a b =
  withFind a $ \ra@(TV ri) -> withFind b $ \rb@(TV rj) ->
    when (ra /= rb) $ do
      (rankA, mA) <- readRoot ra
      (rankB, mB) <- readRoot rb
      merged <-
        (mA, mB) & \case
          (Just tA, Just tB) -> unify [] tA tB $> mA
          _ -> pure $ mA <|> mB
      withStore $ \v ->
        case compare rankA rankB of
          LT -> do
            Vector.Mutable.write v ri $ UFLink rb
            Vector.Mutable.write v rj $ UFRoot rankB merged
          GT -> do
            Vector.Mutable.write v rj $ UFLink ra
            Vector.Mutable.write v ri $ UFRoot rankA merged
          EQ -> do
            Vector.Mutable.write v rj $ UFLink ra
            Vector.Mutable.write v ri $ UFRoot (rankA + 1) merged

-- | Bind a root type variable to a concrete (non-'TVar') type.
-- If the root already has a resolved type, unifies it with @t@
-- for consistency instead of overwriting.
ufBind :: TV -> InfernoType -> Infer s ()
ufBind tv t = withFind tv $ \root@(TV ri) ->
  readRoot root >>= \case
    (rank, Nothing) ->
      withStore $ \v -> Vector.Mutable.write v ri . UFRoot rank $ Just t
    (_, Just old) ->
      unify [] old t

-- | Walk a type, replacing 'TVar's via 'ufProbe' and recursing structurally.
-- For 'TRecord', when the row variable resolves to another record, merge
-- the fields and continue zonking the new row variable (mirrors
-- @apply (Subst s) (TRecord ...)@ in @Syntax.hs@).
zonk :: InfernoType -> Infer s InfernoType
zonk = \case
  TVar tv ->
    ufProbe tv >>= \case
      t@(TVar _) -> pure t
      t -> zonk t
  TBase b -> pure $ TBase b
  TArr a b -> TArr <$> zonk a <*> zonk b
  TArray t -> TArray <$> zonk t
  TSeries t -> TSeries <$> zonk t
  TOptional t -> TOptional <$> zonk t
  TTuple ts -> TTuple <$> traverse zonk ts
  TRep t -> TRep <$> zonk t
  TRecord fields ror -> (`zonkRecord` ror) =<< traverse zonk fields
  where
    -- Zonk a record's row variable; if it resolves to another @TRecord@,
    -- zonk the new fields, merge, and keep going.
    zonkRecord :: Map Ident InfernoType -> RestOfRecord -> Infer s InfernoType
    zonkRecord fs = \case
      RowAbsent -> pure $ TRecord fs RowAbsent
      RowVar tv ->
        ufProbe tv >>= \case
          TVar v -> pure . TRecord fs $ RowVar v
          TRecord extra ror ->
            (`zonkRecord` ror) . Map.union fs =<< traverse zonk extra
          -- Row variable resolved to something that is not a record or a var;
          -- this should not happen in well-formed programs. Treat as opaque.
          _ -> pure $ TRecord fs RowAbsent

-- | Zonk an 'ImplType': zonk both the implicit map values and the body.
zonkImplType :: ImplType -> Infer s ImplType
zonkImplType (ImplType impl body) =
  ImplType <$> traverse zonk impl <*> zonk body

-- | Zonk each parameter of a 'TypeClass'.
zonkTC :: TypeClass -> Infer s TypeClass
zonkTC (TypeClass nm tys) = TypeClass nm <$> traverse zonk tys

-- | Check whether @tv@ occurs in @t@ (after zonking).
occursIn :: TV -> InfernoType -> Infer s Bool
occursIn tv t = (tv `Set.member`) . ftv <$> zonk t

-------------------------------------------------------------------------------
-- Type Map and Environment Helpers
-------------------------------------------------------------------------------

-- | Extend the typing environment for a local binding (scoped via @local@).
inEnv :: (ExtIdent, TypeMetadata TCScheme) -> Infer s a -> Infer s a
inEnv (x, meta) =
  local $ \ctx -> ctx{env = Env.remove ctx.env x `Env.extend` (x, meta)}

-- | Record a type metadata entry for the given source location.
attachTypeToPosition ::
  Location SourcePos ->
  TypeMetadata (Set TypeClass, ImplType) ->
  Infer s ()
attachTypeToPosition k meta =
  liftST . (`modifySTRef'` Map.insert k meta)
    =<< asks (.refs.typeMap)

-- | Merge a list of implicit maps. When two maps share a key, 'unify' the
-- types immediately (eager unification replaces the old constraint-based
-- approach).
mergeImplMaps ::
  forall s.
  Location SourcePos ->
  [Map ExtIdent InfernoType] ->
  Infer s (Map ExtIdent InfernoType)
mergeImplMaps loc =
  flip foldl' (pure mempty) $
    \acc m -> merge m =<< acc
  where
    merge ::
      Map ExtIdent InfernoType ->
      Map ExtIdent InfernoType ->
      Infer s (Map ExtIdent InfernoType)
    merge m merged = do
      for_ (Map.toList (Map.intersectionWith (,) merged m)) $
        \(ident, (t1, t2)) ->
          unify [ImplicitVarTypeOverlap mempty ident t1 t2 loc] t1 t2
      pure $ Map.union merged m

-- | Check for duplicate field names in a record literal.
checkDuplicateFields ::
  Location SourcePos ->
  [(Ident, a, b)] ->
  Infer s ()
checkDuplicateFields loc = go mempty
  where
    go :: Set Ident -> [(Ident, a, b)] -> Infer s ()
    go = \cases
      _ [] -> pure ()
      seen ((f, _, _) : rest)
        | Set.member f seen -> throwError [DuplicateRecordField f loc]
        | otherwise -> go (Set.insert f seen) rest

-- | Defer a typeclass constraint for later resolution. Appends to the
-- shared @deferred@ list; 'resolveTypeClasses' processes these after
-- the full inference traversal.
deferTC :: Location SourcePos -> TypeClass -> Infer s ()
deferTC loc tc =
  liftST . (`modifySTRef'` ((loc, tc) :))
    =<< asks (.refs.deferred)

-- | Look up a variable (by hash or name) in the environment, instantiate
-- its scheme, and return the metadata with the instantiated type.
lookupEnv ::
  Location SourcePos ->
  Either VCObjectHash ExtIdent ->
  Infer s (TypeMetadata (Set TypeClass, ImplType))
lookupEnv loc x =
  asks (.env) >>= \env ->
    case either (`Env.lookupPinned` env) (`Env.lookup` env) x of
      Nothing ->
        throwError
          [ either
              (flip (UnboundNameInNamespace LocalScope) loc . Left)
              (flip (UnboundExtIdent LocalScope) loc)
              x
          ]
      Just meta -> instantiate meta.ty <&> \ty -> meta{ty}

-- | Look up a pinned hash that must exist (e.g. op vars, enums).
-- If the 'Pinned' value is 'Local' (not pinned), throws
-- 'UnboundNameInNamespace' with the given 'Namespace' for a proper error.
lookupPinned ::
  Location SourcePos ->
  Pinned VCObjectHash ->
  Namespace ->
  Infer s (TypeMetadata (Set TypeClass, ImplType))
lookupPinned loc pin ns =
  maybe
    (throwError [UnboundNameInNamespace LocalScope (Right ns) loc])
    (lookupEnv loc . Left)
    $ pinnedToMaybe pin

-- | Instantiate a type scheme by replacing each quantified variable
-- with a fresh UF cell. The scheme body is pure ('ImplType'), so we
-- build a local 'Subst' and 'apply' it.
instantiate :: TCScheme -> Infer s (Set TypeClass, ImplType)
instantiate (ForallTC as tcs t) =
  traverse (const (fmap TVar freshTV)) as <&> \freshVars ->
    let sub :: Subst
        sub = Subst . Map.fromList $ zip as freshVars
     in (Set.map (apply sub) tcs, apply sub t)

-------------------------------------------------------------------------------
-- Core Unification
-------------------------------------------------------------------------------

-- | Unify two types eagerly using the union-find. The error list is used when
-- unification fails.
unify :: [TypeError SourcePos] -> InfernoType -> InfernoType -> Infer s ()
unify err t1 t2 = join $ go <$> zonk t1 <*> zonk t2
  where
    go :: InfernoType -> InfernoType -> Infer s ()
    go = \cases
      a b | a == b -> pure ()
      (TVar a) (TVar b) -> ufUnion a b
      (TVar a) t -> do
        whenM (occursIn a t) $ throwError infiniteErrs
        ufBind a t
      t (TVar a) -> do
        whenM (occursIn a t) $ throwError infiniteErrs
        ufBind a t
      (TArr a1 b1) (TArr a2 b2) -> unify err a1 a2 *> unify err b1 b2
      (TArray a) (TArray b) -> unify err a b
      (TSeries a) (TSeries b) -> unify err a b
      (TOptional a) (TOptional b) -> unify err a b
      (TRep a) (TRep b) -> unify err a b
      (TTuple ts1) (TTuple ts2)
        | length ts1 == length ts2 ->
            zipWithM_ (unify err) (toList ts1) $ toList ts2
      (TRecord fs1 r1) (TRecord fs2 r2) ->
        unifyRecordFields
          err
          RecordSide{fields = Map.toAscList fs1, ror = r1, new = mempty}
          RecordSide{fields = Map.toAscList fs2, ror = r2, new = mempty}
          mempty
      _ _ -> throwError err

    -- Extract `InfiniteType` errors; fall back to the full error list
    infiniteErrs :: [TypeError SourcePos]
    infiniteErrs
      | not $ any isInfiniteType err = err
      | otherwise = errs
      where
        errs :: [TypeError SourcePos]
        errs = filter isInfiniteType err

    isInfiniteType :: TypeError SourcePos -> Bool
    isInfiniteType = \case
      InfiniteType{} -> True
      _ -> False

-- | Unify the fields of two record types.
--
-- Same mergesort-style algorithm as the old 'unifyRecords', but calls
-- 'unify' directly instead of composing substitutions.
--
-- The idea: given two records, recursively walk their sorted field lists.
-- Common fields have their types unified. Fields present in only one record
-- create fresh type variables on the other side (expanding the row variable).
-- At the base case, row variables are unified and any accumulated new fields
-- are bound to the row variables via 'ufBind'.
unifyRecordFields ::
  [TypeError SourcePos] ->
  RecordSide ->
  RecordSide ->
  [(InfernoType, InfernoType)] ->
  Infer s ()
unifyRecordFields err = \cases
  -- Base case: all fields consumed
  s1@RecordSide{fields = []} s2@RecordSide{fields = []} pairs -> do
    -- Unify all accumulated field-type pairs
    for_ pairs . uncurry $ unify err
    -- Build new record types for the row variable expansions, then unify
    -- the row variables
    join $
      unifyRowVars err
        <$> expandRowVar err s1.ror s1.new
        <*> expandRowVar err s2.ror s2.new
  -- Left exhausted, right has fields remaining
  s1@RecordSide{fields = []} s2@RecordSide{fields = (f2, t2) : ts2} pairs ->
    (freshTV >>=) . (. TVar) $ \tv ->
      unifyRecordFields
        err
        s1{new = (f2, tv) : s1.new}
        s2{fields = ts2}
        $ (tv, t2) : pairs
  -- Right exhausted, left has fields remaining
  s1@RecordSide{fields = (f1, t1) : ts1} s2@RecordSide{fields = []} pairs ->
    (freshTV >>=) . (. TVar) $ \tv ->
      unifyRecordFields
        err
        s1{fields = ts1}
        s2{new = (f1, tv) : s2.new}
        $ (tv, t1) : pairs
  -- Both have fields; compare the smallest field names
  s1@RecordSide{fields = (f1, t1) : ts1} s2@RecordSide{fields = (f2, t2) : ts2} pairs
    | f1 == f2 ->
        unifyRecordFields err s1{fields = ts1} s2{fields = ts2} $ (t1, t2) : pairs
    | f1 > f2 ->
        (freshTV >>=) . (. TVar) $ \tv ->
          unifyRecordFields
            err
            s1{new = (f2, tv) : s1.new}
            s2{fields = ts2}
            $ (tv, t2) : pairs
    | otherwise ->
        (freshTV >>=) . (. TVar) $ \tv ->
          unifyRecordFields
            err
            s1{fields = ts1}
            s2{new = (f1, tv) : s2.new}
            $ (tv, t1) : pairs

-- | If there are new fields to add, bind the row variable to a record
-- containing those fields and a fresh row variable. Returns the new
-- 'RestOfRecord' tail.
expandRowVar ::
  [TypeError SourcePos] ->
  RestOfRecord ->
  [(Ident, InfernoType)] ->
  Infer s RestOfRecord
expandRowVar = \cases
  _ ror [] -> pure ror
  err RowAbsent (_ : _) -> throwError err
  _ (RowVar tv) newFields ->
    freshTV >>= \tailVar ->
      -- Bind the old row var to a record of the new fields + fresh tail
      ufBind tv (TRecord (Map.fromDescList newFields) (RowVar tailVar))
        $> RowVar tailVar

-- | Unify two 'RestOfRecord' tails.
unifyRowVars :: [TypeError SourcePos] -> RestOfRecord -> RestOfRecord -> Infer s ()
unifyRowVars = \cases
  _ RowAbsent RowAbsent -> pure ()
  err (RowVar tv) RowAbsent ->
    unify err (TVar tv) $ TRecord mempty RowAbsent
  err RowAbsent (RowVar tv) ->
    unify err (TVar tv) $ TRecord mempty RowAbsent
  err (RowVar tv1) (RowVar tv2) ->
    unify err (TVar tv1) $ TVar tv2

-------------------------------------------------------------------------------
-- Public `unifyRecords` (Pure, Self-Contained)
-------------------------------------------------------------------------------

-- | Unify two record types purely. Creates a local UF in 'runST',
-- runs the record unification algorithm, and freezes the result to a 'Subst'.
unifyRecords ::
  [TypeError SourcePos] ->
  (Map Ident InfernoType, RestOfRecord) ->
  (Map Ident InfernoType, RestOfRecord) ->
  Either [TypeError SourcePos] Subst
unifyRecords err (fs1, ror1) (fs2, ror2) = runST go
  where
    -- Compute the starting counter from the max TV in the inputs
    maxTV :: Int
    maxTV =
      maybe 0 ((+ 1) . unTV) . Set.lookupMax $
        ftv (TRecord fs1 ror1) <> ftv (TRecord fs2 ror2)

    go :: forall s. ST s (Either [TypeError SourcePos] Subst)
    go = do
      cRef <- newSTRef maxTV
      vecInit <- Vector.Mutable.replicate (max 128 (maxTV * 2)) $ UFRoot 0 Nothing
      stRef <- newSTRef vecInit
      tmRef <- newSTRef mempty
      modsRef <- newSTRef mempty
      patsRef <- newSTRef mempty
      defRef <- newSTRef mempty
      let refs :: InferRefs s
          refs =
            InferRefs
              { count = cRef
              , ufStore = stRef
              , typeMap = tmRef
              , modules = modsRef
              , tyClasses = mempty
              , patternsToCheck = patsRef
              , deferred = defRef
              }

          ctx :: InferCtx s
          ctx = InferCtx{env = mempty, refs = refs}

          doUnify :: Infer s ()
          doUnify =
            unifyRecordFields
              err
              RecordSide
                { fields = Map.toAscList fs1
                , ror = ror1
                , new = mempty
                }
              RecordSide
                { fields = Map.toAscList fs2
                , ror = ror2
                , new = mempty
                }
              mempty

          freeze :: ST s (Either [TypeError SourcePos] Subst)
          freeze = join $ freezeUFToSubst <$> readSTRef cRef <*> readSTRef stRef

      either (pure . Left) (const freeze) =<< runExceptT (runReaderT doUnify ctx)

-- | Freeze the UF store into a 'Subst'. For each TV @0..n-1@, fully
-- resolve its type (transitively resolving all inner @TVar@s) and add
-- the mapping if it differs from the identity.
freezeUFToSubst ::
  forall s.
  Int ->
  STVector s UFContent ->
  ST s (Either [TypeError SourcePos] Subst)
freezeUFToSubst n v =
  Right . Subst . Map.fromList <$> go 0
  where
    go :: Int -> ST s [(TV, InfernoType)]
    go i
      | i >= n = pure mempty
      | otherwise = do
          t <- resolveType =<< findRoot i
          rest <- go $ i + 1
          t & \case
            TVar tv
              | tv == TV i -> pure rest
            _ -> pure $ (TV i, t) : rest

    -- Chase @UFLink@ pointers to the root and return its stored type
    findRoot :: Int -> ST s InfernoType
    findRoot i =
      Vector.Mutable.read v i >>= \case
        UFLink (TV j) -> findRoot j
        UFRoot _ (Just t) -> pure t
        UFRoot _ Nothing -> pure . TVar $ TV i

    -- Transitively resolve all @TVar@s within a type
    resolveType :: InfernoType -> ST s InfernoType
    resolveType = \case
      TVar tv@(TV i) ->
        findRoot i >>= \case
          TVar u | u == tv -> pure $ TVar tv
          t -> resolveType t
      TBase b -> pure $ TBase b
      TArr a b -> TArr <$> resolveType a <*> resolveType b
      TArray t -> TArray <$> resolveType t
      TSeries t -> TSeries <$> resolveType t
      TOptional t -> TOptional <$> resolveType t
      TTuple ts -> TTuple <$> traverse resolveType ts
      TRep t -> TRep <$> resolveType t
      TRecord fs ror -> (`resolveRecord` ror) =<< traverse resolveType fs

    -- Resolve a record's row variable tail, merging fields from any
    -- row variable expansion (mirrors @zonkRecord@ in @zonk@)
    resolveRecord :: Map Ident InfernoType -> RestOfRecord -> ST s InfernoType
    resolveRecord fs = \case
      RowAbsent -> pure $ TRecord fs RowAbsent
      RowVar tv@(TV i) ->
        findRoot i >>= \case
          TVar u | u == tv -> pure . TRecord fs $ RowVar tv
          TRecord extra ror ->
            (`resolveRecord` ror) . Map.union fs =<< traverse resolveType extra
          _ -> pure $ TRecord fs RowAbsent

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Top-level inference dispatcher. Pattern-matches on the expression
-- constructor and delegates to the appropriate helper.
infer :: Expr (Pinned VCObjectHash) SourcePos -> Infer s InferResult
infer expr = case expr of
  -- Literals: ints get a @numeric@ typeclass constraint; others are ground
  --
  -- NOTE: due to constraint above `int` inference MUST come before other
  -- `Lit` inference. Do NOT re-order these two or the typechecker will fail
  Lit pos l@(LInt _) -> inferLitInt expr loc pos l
  Lit _ l -> inferLitOther expr loc l
  Var pos pin _ (Expl x) -> inferVarExpl expr loc pos pin x
  Var _ _ _ (Impl x) -> inferVarImpl expr loc x
  OpVar _ pin _ ident -> inferOpVar expr loc pin ident
  Enum _ pin _ ident -> inferEnum expr loc pin ident
  InterpolatedString p xs end -> inferInterp expr loc p xs end
  Record open fes close -> inferRecord expr loc open fes close
  RecordField pos recNm fieldNm -> inferRecField expr loc pos recNm fieldNm
  Array open elems close -> inferArray expr loc open elems close
  ArrayComp open body pipe rawSels cond close ->
    inferArrayComp
      loc
      ArrayCompBinding
        { open
        , body
        , pipe
        , cond
        , close
        , sels = fmap toSel rawSels
        }
  Lam funPos args arrowPos body -> inferLam funPos args arrowPos body
  App e1 e2 -> inferApp loc e1 e2
  Let letPos varPos (Expl ident) eqPos rhs inPos inExpr ->
    inferLet
      loc
      LetBinding
        { letPos
        , varPos
        , ident
        , eqPos
        , rhs
        , inPos
        , inExpr
        }
  Let letPos varPos (Impl ident) eqPos rhs inPos inExpr ->
    inferLetImpl
      loc
      LetBinding
        { letPos
        , varPos
        , ident
        , eqPos
        , rhs
        , inPos
        , inExpr
        }
  LetAnnot letPos varPos ident annotPos scheme eqPos rhs inPos inExpr ->
    inferLetAnnot
      loc
      LetAnnotBinding
        { letPos
        , varPos
        , ident
        , annotPos
        , scheme
        , eqPos
        , rhs
        , inPos
        , inExpr
        }
  Op lhs opPos pin opMeta modNm op rhs ->
    inferOp
      loc
      OpBinding
        { lhs
        , opPos
        , pin
        , opMeta
        , modNm
        , op
        , rhs
        }
  PreOp opPos pin opLvl modNm op operand ->
    inferPreOp
      loc
      PreOpBinding
        { opPos
        , pin
        , opLvl
        , modNm
        , op
        , operand
        }
  If ifPos cond thenPos tr elsePos fl ->
    inferIf
      loc
      IfBinding{ifPos, cond, thenPos, tr, elsePos, fl}
  Tuple open elems close -> inferTuple expr loc open elems close
  Assert assertPos cond inPos e -> inferAssert loc assertPos cond inPos e
  Case matchPos scrut openPos rawBranches closePos ->
    inferCase loc scrut (fmap toBranch rawBranches) <&> \cr ->
      InferResult
        { expr =
            Case matchPos cr.scrutExpr openPos (fmap fromBranch cr.branches) closePos
        , typ = cr.typ
        , tcs = cr.tcs
        }
  One pos e -> inferOne loc pos e
  Empty _ -> inferEmpty expr loc
  RenameModule namePos newNm oldPos oldNm inPos body ->
    inferRenameModule
      RenameModuleBinding
        { namePos
        , newNm
        , oldPos
        , oldNm
        , inPos
        , body
        }
  OpenModule openPos pin modNm imports inPos body ->
    inferOpenModule
      OpenModuleBinding
        { openPos
        , pin
        , modNm
        , imports
        , inPos
        , body
        }
  TypeRep _ t -> inferTypeRep expr t
  -- Syntactic wrappers (these are structurally transparent)
  CommentAbove c e -> inferCommentAbove c e
  CommentAfter e c -> inferCommentAfter e c
  CommentBelow e c -> inferCommentBelow e c
  Bracketed p1 e p2 -> inferBracketed p1 e p2
  where
    loc :: Location SourcePos
    loc = blockPosition expr

    toSel ::
      ( SourcePos
      , Ident
      , SourcePos
      , Expr (Pinned VCObjectHash) SourcePos
      , Maybe SourcePos
      ) ->
      Selector
    toSel (identPos, ident, arrowPos, gen, commaPos) =
      Selector
        { identPos
        , ident
        , arrowPos
        , gen
        , commaPos
        }

    toBranch ::
      ( SourcePos
      , Pat (Pinned VCObjectHash) SourcePos
      , SourcePos
      , Expr (Pinned VCObjectHash) SourcePos
      ) ->
      CaseBranch
    toBranch (barPos, pat, arrPos, body) =
      CaseBranch
        { barPos
        , pat
        , arrPos
        , body
        }

    fromBranch ::
      CaseBranch ->
      ( SourcePos
      , Pat (Pinned VCObjectHash) SourcePos
      , SourcePos
      , Expr (Pinned VCObjectHash) SourcePos
      )
    fromBranch b = (b.barPos, b.pat, b.arrPos, b.body)

-------------------------------------------------------------------------------
-- Inference Helpers
-------------------------------------------------------------------------------

-- | Infer an integer literal. Creates a fresh type variable constrained by
-- the @numeric@ typeclass, and wraps the expression with an implicit @TRep@
-- argument (used for runtime representation dispatch).
inferLitInt ::
  Expr (Pinned VCObjectHash) SourcePos ->
  Location SourcePos ->
  SourcePos ->
  Lit ->
  Infer s InferResult
inferLitInt expr loc pos l = do
  tv <- TVar <$> freshTV
  i <- ExtIdent . Left <$> freshTVRaw

  let tyCls :: TypeClass
      tyCls = TypeClass "numeric" [tv]

  attachTypeToPosition loc $
    TypeMetadata
      { identExpr = Lit () l
      , ty = (Set.singleton tyCls, ImplType mempty tv)
      , docs = Nothing
      }
  pure
    InferResult
      { expr = App expr . Var pos Local LocalScope $ Impl i
      , typ = flip ImplType tv . Map.singleton i $ TRep tv
      , tcs = Set.singleton tyCls
      }

-- | Infer a non-integer literal (@LDouble@, @LHex@, @LText@).
-- The type is known ground; no typeclass constraint is needed.
inferLitOther ::
  Expr (Pinned VCObjectHash) SourcePos ->
  Location SourcePos ->
  Lit ->
  Infer s InferResult
inferLitOther expr loc l = do
  attachTypeToPosition loc $
    TypeMetadata
      { identExpr = Lit () l
      , ty = (mempty, ImplType mempty t)
      , docs = Nothing
      }
  pure
    InferResult
      { expr
      , typ = ImplType mempty t
      , tcs = mempty
      }
  where
    t :: InfernoType
    t = litType l

    litType :: Lit -> InfernoType
    litType = \case
      LDouble _ -> typeDouble
      LHex _ -> typeWord64
      LText _ -> typeText
      -- This is ugly but necessary. Int literals really must be inferred first,
      -- see `infer`
      LInt _ ->
        error "internal typechecker error: ints MUST be inferred before other literals"

-- | Infer an explicit variable reference. Looks up its type scheme in the
-- environment, instantiates it, and handles @rep@ typeclass constraints by
-- creating implicit @TRep@ arguments.
inferVarExpl ::
  Expr (Pinned VCObjectHash) SourcePos ->
  Location SourcePos ->
  SourcePos ->
  Pinned VCObjectHash ->
  ExtIdent ->
  Infer s InferResult
inferVarExpl expr loc pos mHash x = do
  meta <- lookupEnv loc $ maybe (Right x) Left $ pinnedToMaybe mHash
  let tcs :: Set TypeClass
      tcs = fst meta.ty

      iType :: ImplType
      iType = snd meta.ty
  attachTypeToPosition loc meta
  let repAndNon :: (Set TypeClass, Set TypeClass)
      repAndNon = Set.partition isRepTC tcs
      (repTcs, nonRepTcs) = repAndNon
  Set.lookupMin repTcs & \case
    Just (TypeClass _ repTys) ->
      traverse mkRepImpl repTys <&> \repImpls ->
        InferResult
          { expr =
              foldl' App expr $
                fmap (Var pos Local LocalScope . Impl . fst) repImpls
          , typ = ImplType (iType.impl `Map.union` Map.fromList repImpls) iType.body
          , tcs = nonRepTcs
          }
    Nothing ->
      pure
        InferResult
          { expr
          , typ = iType
          , tcs = nonRepTcs
          }
  where
    isRepTC :: TypeClass -> Bool
    isRepTC (TypeClass nm _) = nm == "rep"

    mkRepImpl :: InfernoType -> Infer s (ExtIdent, InfernoType)
    mkRepImpl repTy =
      (,TRep repTy) . ExtIdent . Left <$> freshTVRaw

-- | Infer an implicit variable reference. Creates a fresh type variable
-- and records it in the implicit map.
inferVarImpl ::
  Expr (Pinned VCObjectHash) SourcePos ->
  Location SourcePos ->
  ExtIdent ->
  Infer s InferResult
inferVarImpl expr loc x = do
  tv <- TVar <$> freshTV
  let implTy :: ImplType
      implTy = ImplType (Map.singleton x tv) tv
  attachTypeToPosition loc $
    TypeMetadata
      { identExpr = bimap (const ()) (const ()) expr
      , ty = (mempty, implTy)
      , docs = Nothing
      }
  pure
    InferResult
      { expr
      , typ = implTy
      , tcs = mempty
      }

-- | Infer a prefix-used operator (e.g. @(+)@). Op vars must always be pinned.
inferOpVar ::
  Expr (Pinned VCObjectHash) SourcePos ->
  Location SourcePos ->
  Pinned VCObjectHash ->
  Ident ->
  Infer s InferResult
inferOpVar expr loc pin ident =
  lookupPinned loc pin (OpNamespace ident) >>= \meta -> do
    attachTypeToPosition loc meta
    pure
      InferResult
        { expr
        , typ = snd meta.ty
        , tcs = fst meta.ty
        }

-- | Infer an enum constructor. Enums must always be pinned.
inferEnum ::
  Expr (Pinned VCObjectHash) SourcePos ->
  Location SourcePos ->
  Pinned VCObjectHash ->
  Ident ->
  Infer s InferResult
inferEnum expr loc pin ident =
  lookupPinned loc pin (EnumNamespace ident) >>= \meta -> do
    attachTypeToPosition
      loc
      meta
        { identExpr = bimap (const ()) (const ()) expr
        }
    pure
      InferResult
        { expr
        , typ = snd meta.ty
        , tcs = mempty
        }

-- | Infer an interpolated string. Each embedded expression is inferred
-- independently; the overall type is @text@. Implicit maps are merged eagerly.
inferInterp ::
  Expr (Pinned VCObjectHash) SourcePos ->
  Location SourcePos ->
  SourcePos ->
  SomeIStr (SourcePos, Expr (Pinned VCObjectHash) SourcePos, SourcePos) ->
  SourcePos ->
  Infer s InferResult
inferInterp expr loc p xs end = do
  attachTypeToPosition loc $
    TypeMetadata
      { identExpr = bimap (const ()) (const ()) $ removeComments expr
      , ty = (mempty, ImplType mempty typeText)
      , docs = Nothing
      }

  (parts, impls, tcSets) <- fmap unzip3 . traverse inferPart $ toEitherList xs
  merged <- mergeImplMaps loc impls
  pure
    InferResult
      { expr = InterpolatedString p (fromEitherList parts) end
      , typ = ImplType merged typeText
      , tcs = mconcat tcSets
      }
  where
    inferPart ::
      Either
        Text
        ( SourcePos
        , Expr (Pinned VCObjectHash) SourcePos
        , SourcePos
        ) ->
      Infer
        s
        ( Either Text (SourcePos, Expr (Pinned VCObjectHash) SourcePos, SourcePos)
        , Map ExtIdent InfernoType
        , Set TypeClass
        )
    inferPart = \case
      Left str -> pure (Left str, mempty, mempty)
      Right (p1, e, p2) ->
        infer e <&> \r -> (Right (p1, r.expr, p2), r.typ.impl, r.tcs)

-- | Infer a record literal @{ f1 = e1; f2 = e2; ... }@. Each field
-- expression is inferred; implicit maps are merged eagerly.
inferRecord ::
  Expr (Pinned VCObjectHash) SourcePos ->
  Location SourcePos ->
  SourcePos ->
  [(Ident, Expr (Pinned VCObjectHash) SourcePos, Maybe SourcePos)] ->
  SourcePos ->
  Infer s InferResult
inferRecord expr loc open fes close = do
  checkDuplicateFields loc fes
  results <- traverse (infer . snd3) fes
  merged <- mergeImplMaps loc $ fmap (.typ.impl) results

  let names :: [Ident]
      names = fmap fst3 fes

      fieldTypes :: Map Ident InfernoType
      fieldTypes = Map.fromList $ zip names (fmap (.typ.body) results)

      rebuiltFes :: [(Ident, Expr (Pinned VCObjectHash) SourcePos, Maybe SourcePos)]
      rebuiltFes = zipWith rebuildFe fes results

      rebuildFe ::
        (Ident, a, Maybe SourcePos) ->
        InferResult ->
        (Ident, Expr (Pinned VCObjectHash) SourcePos, Maybe SourcePos)
      rebuildFe fe r = (fst3 fe, r.expr, thd3 fe)

      recTy :: ImplType
      recTy = ImplType merged $ TRecord fieldTypes RowAbsent

  attachTypeToPosition loc $
    TypeMetadata
      { identExpr = bimap (const ()) (const ()) $ removeComments expr
      , ty = (mempty, recTy)
      , docs = Nothing
      }

  pure
    InferResult
      { expr = Record open rebuiltFes close
      , typ = recTy
      , tcs = foldMap (.tcs) results
      }

-- | Infer a record field access @r.f@. Synthesizes a record type with
-- the accessed field and a fresh row variable, then unifies it with the
-- inferred type of @r@.
inferRecField ::
  Expr (Pinned VCObjectHash) SourcePos ->
  Location SourcePos ->
  SourcePos ->
  Ident ->
  Ident ->
  Infer s InferResult
inferRecField expr loc pos (Ident recN) fieldName = do
  r <- infer . Var pos Local LocalScope . Expl . ExtIdent $ Right recN
  fieldTv <- TVar <$> freshTV
  rowTv <- freshTV

  let recTy :: InfernoType
      recTy = TRecord (Map.singleton fieldName fieldTv) $ RowVar rowTv

  unify [UnificationFail r.tcs r.typ.body recTy loc] r.typ.body recTy
  pure
    InferResult
      { expr
      , typ = ImplType r.typ.impl fieldTv
      , tcs = r.tcs
      }

-- | Infer an array literal. Creates a fresh element type variable and
-- unifies every element against it (handles empty arrays uniformly).
inferArray ::
  Expr (Pinned VCObjectHash) SourcePos ->
  Location SourcePos ->
  SourcePos ->
  [(Expr (Pinned VCObjectHash) SourcePos, Maybe SourcePos)] ->
  SourcePos ->
  Infer s InferResult
inferArray expr loc open elems close = do
  elemTv <- TVar <$> freshTV
  results <- for elems $ inferElem elemTv
  merged <- mergeImplMaps loc $ fmap (.typ.impl) results

  let arrTy :: ImplType
      arrTy = ImplType merged $ TArray elemTv

  attachTypeToPosition loc $
    TypeMetadata
      { identExpr = bimap (const ()) (const ()) $ removeComments expr
      , ty = (mempty, arrTy)
      , docs = Nothing
      }
  pure
    InferResult
      { expr = Array open (zip (fmap (.expr) results) (fmap snd elems)) close
      , typ = arrTy
      , tcs = foldMap (.tcs) results
      }
  where
    inferElem ::
      InfernoType ->
      (Expr (Pinned VCObjectHash) SourcePos, Maybe SourcePos) ->
      Infer s InferResult
    inferElem tv (e, _) =
      infer e >>= \r ->
        r
          <$ unify [UnificationFail r.tcs tv r.typ.body $ blockPosition e] tv r.typ.body

-- | Infer an array comprehension @[body | x <- gen, ... if cond]@.
-- Each selector binds a variable into scope for subsequent selectors,
-- the body, and the optional condition.
inferArrayComp :: Location SourcePos -> ArrayCompBinding -> Infer s InferResult
inferArrayComp loc ac = do
  checkVarOverlap selList mempty

  cr <- processSels selList
  merged <- mergeImplMaps loc $ cr.bodyResult.typ.impl : cr.condImpl : cr.selImpls
  rebuiltSels <-
    toList cr.rebuiltSels & \case
      h : t -> pure $ fmap fromSelector (h :| t)
      -- Not possible in practice
      [] -> throwError []

  pure
    InferResult
      { expr = ArrayComp ac.open cr.bodyResult.expr ac.pipe rebuiltSels cr.condExpr ac.close
      , typ = ImplType merged . TArray $ cr.bodyResult.typ.body
      , tcs = cr.bodyResult.tcs <> cr.condTcs <> cr.selTcs
      }
  where
    selList :: [Selector]
    selList = toList ac.sels

    -- Check for duplicate variable names in selectors. Uses a `Map` to track
    -- first-occurrence locations for O(n log n) instead of O(n^2).
    checkVarOverlap ::
      [Selector] ->
      Map Ident (Location SourcePos) ->
      Infer s ()
    checkVarOverlap [] _ = pure ()
    checkVarOverlap (sel : rest) seen =
      case Map.lookup sel.ident seen of
        Just prev ->
          throwError
            [ VarMultipleOccurrence sel.ident prev $
                elementPosition sel.identPos sel.ident
            ]
        Nothing ->
          checkVarOverlap rest
            . flip (Map.insert sel.ident) seen
            $ elementPosition sel.identPos sel.ident

    -- Process selectors left-to-right, nesting `inEnv` calls so each
    -- subsequent selector (and the body/condition) sees all prior bindings.
    -- Returns a `CompResult` with rebuilt selectors in forward order
    -- (built during unwinding via `(<|)`).
    processSels :: [Selector] -> Infer s CompResult
    processSels = \case
      [] -> do
        rBody <- infer ac.body
        (ce, ci, ct) <- inferCond
        pure
          CompResult
            { rebuiltSels = mempty
            , selImpls = []
            , selTcs = mempty
            , bodyResult = rBody
            , condExpr = ce
            , condImpl = ci
            , condTcs = ct
            }
      (sel : rest) -> do
        (rGen, _, binding) <- inferSel sel
        inEnv binding $
          processSels rest <&> \cr ->
            cr
              { rebuiltSels = sel{gen = rGen.expr} <| cr.rebuiltSels
              , selImpls = rGen.typ.impl : cr.selImpls
              , selTcs = rGen.tcs <> cr.selTcs
              }

    -- Infer a single selector: infer the generator, unify with `TArray tv`,
    -- attach metadata, and build the environment binding.
    inferSel ::
      Selector ->
      Infer s (InferResult, InfernoType, (ExtIdent, TypeMetadata TCScheme))
    inferSel sel = do
      rGen <- infer sel.gen
      tv <- TVar <$> freshTV
      unify
        [UnificationFail rGen.tcs rGen.typ.body (TArray tv) $ blockPosition sel.gen]
        rGen.typ.body
        (TArray tv)

      let Ident x = sel.ident
          xExt :: ExtIdent
          xExt = ExtIdent $ Right x

          varExpr :: Expr () ()
          varExpr = Var () () LocalScope . Expl . ExtIdent $ Right x

      attachTypeToPosition (elementPosition sel.identPos sel.ident) $
        TypeMetadata
          { identExpr = varExpr
          , ty = (rGen.tcs, ImplType rGen.typ.impl tv)
          , docs = Nothing
          }
      pure
        ( rGen
        , tv
        ,
          ( xExt
          , TypeMetadata
              { identExpr = varExpr
              , ty = ForallTC [] rGen.tcs $ ImplType rGen.typ.impl tv
              , docs = Nothing
              }
          )
        )

    -- Infer the optional condition expression, unifying with `bool`.
    inferCond ::
      Infer
        s
        ( Maybe (SourcePos, Expr (Pinned VCObjectHash) SourcePos)
        , Map ExtIdent InfernoType
        , Set TypeClass
        )
    inferCond = case ac.cond of
      Nothing -> pure (Nothing, mempty, mempty)
      Just (p, eCond) -> do
        rCond <- infer eCond
        unify
          [UnificationFail rCond.tcs rCond.typ.body typeBool $ blockPosition eCond]
          rCond.typ.body
          typeBool
        pure (Just (p, rCond.expr), rCond.typ.impl, rCond.tcs)

    fromSelector ::
      Selector ->
      (SourcePos, Ident, SourcePos, Expr (Pinned VCObjectHash) SourcePos, Maybe SourcePos)
    fromSelector s = (s.identPos, s.ident, s.arrowPos, s.gen, s.commaPos)

-- | Infer a lambda expression @fun x y z -> body@. Creates fresh type
-- variables for each parameter, extends the environment for named params,
-- and builds the resulting arrow type from right to left.
inferLam ::
  SourcePos ->
  NonEmpty (SourcePos, Maybe ExtIdent) ->
  SourcePos ->
  Expr (Pinned VCObjectHash) SourcePos ->
  Infer s InferResult
inferLam funPos args arrowPos bodyExpr =
  processArgs (toList args) <&> \r ->
    InferResult
      { expr = Lam funPos args arrowPos r.expr
      , typ = r.typ
      , tcs = r.tcs
      }
  where
    -- Process parameters left-to-right, nesting `inEnv` for named args.
    -- Builds arrow types right-to-left during unwinding.
    processArgs :: [(SourcePos, Maybe ExtIdent)] -> Infer s InferResult
    processArgs = \case
      [] -> infer bodyExpr
      (pos, mx) : rest -> do
        tv <- TVar <$> freshTV
        r <- maybe id (inEnv . mkBinding tv) mx $ processArgs rest

        attachArgMeta pos mx tv
        pure
          InferResult
            { expr = r.expr
            , typ = ImplType r.typ.impl $ tv `TArr` r.typ.body
            , tcs = r.tcs
            }

    mkBinding :: InfernoType -> ExtIdent -> (ExtIdent, TypeMetadata TCScheme)
    mkBinding tv x =
      ( x
      , TypeMetadata
          { identExpr = Var () () LocalScope $ Expl x
          , ty = ForallTC [] mempty $ ImplType mempty tv
          , docs = Nothing
          }
      )

    -- Attach type metadata for the parameter position. Named (non-internal)
    -- parameters and wildcards get source location entries; internal vars
    -- (generated by the compiler) are skipped via `for_` on `Either`.
    attachArgMeta :: SourcePos -> Maybe ExtIdent -> InfernoType -> Infer s ()
    attachArgMeta pos mx tv = case mx of
      Just (ExtIdent ei) ->
        for_ ei $ \i ->
          attachTypeToPosition (elementPosition pos (Just (Ident i))) $
            TypeMetadata
              { identExpr = Var () () LocalScope . Expl . ExtIdent $ Right i
              , ty = (mempty, ImplType mempty tv)
              , docs = Nothing
              }
      Nothing ->
        attachTypeToPosition (elementPosition @(Maybe Ident) pos Nothing) $
          TypeMetadata
            { identExpr = Var () () LocalScope . Expl . ExtIdent $ Right "_"
            , ty = (mempty, ImplType mempty tv)
            , docs = Nothing
            }

-- | Infer a function application @e1 e2@. Infers both sides, merges implicit
-- maps, then unifies the function type eagerly. Zonks the function body type
-- first to distinguish arrow-mismatch errors from non-function errors.
inferApp ::
  Location SourcePos ->
  Expr (Pinned VCObjectHash) SourcePos ->
  Expr (Pinned VCObjectHash) SourcePos ->
  Infer s InferResult
inferApp loc e1 e2 = do
  r1 <- infer e1
  r2 <- infer e2
  tv <- TVar <$> freshTV
  merged <- mergeImplMaps loc [r1.typ.impl, r2.typ.impl]

  let tcs :: Set TypeClass
      tcs = r1.tcs <> r2.tcs

  zonk r1.typ.body >>= \case
    TArr argTy resTy -> do
      unify [UnificationFail tcs argTy r2.typ.body $ blockPosition e2] argTy r2.typ.body
      unify [UnificationFail tcs resTy tv loc] resTy tv
    fnTy ->
      unify
        [ ExpectedFunction tcs (r2.typ.body `TArr` tv) fnTy $
            blockPosition e1
        ]
        fnTy
        $ r2.typ.body `TArr` tv

  pure
    InferResult
      { expr = App r1.expr r2.expr
      , typ = ImplType merged tv
      , tcs
      }

-- | Infer an explicit let binding @let x = e1 in e2@. The bound variable
-- is non-generalized (monomorphic within @e2@).
inferLet :: Location SourcePos -> LetBinding -> Infer s InferResult
inferLet loc lb = do
  r1 <- infer lb.rhs

  let binding :: (ExtIdent, TypeMetadata TCScheme)
      binding =
        ( lb.ident
        , TypeMetadata
            { identExpr = Var () () LocalScope $ Expl lb.ident
            , ty = ForallTC [] r1.tcs $ ImplType r1.typ.impl r1.typ.body
            , docs = Nothing
            }
        )

  r2 <- inEnv binding $ infer lb.inExpr
  merged <- mergeImplMaps loc [r1.typ.impl, r2.typ.impl]

  attachTypeToPosition (elementPosition lb.varPos (Expl lb.ident)) $
    TypeMetadata
      { identExpr = Var () () LocalScope $ Expl lb.ident
      , ty = (r1.tcs, ImplType r1.typ.impl r1.typ.body)
      , docs = Nothing
      }

  pure
    InferResult
      { expr = Let lb.letPos lb.varPos (Expl lb.ident) lb.eqPos r1.expr lb.inPos r2.expr
      , typ = ImplType merged r2.typ.body
      , tcs = r1.tcs <> r2.tcs
      }

-- | Infer an implicit let binding @let ?x = e1 in e2@. If @?x@ appears in
-- @e2@'s implicit map, its type is unified with @e1@'s body type.
inferLetImpl :: Location SourcePos -> LetBinding -> Infer s InferResult
inferLetImpl loc lb = do
  r1 <- infer lb.rhs
  r2 <- infer lb.inExpr

  -- If `x` is in `r2`'s implicits, unify with `r1`'s body; otherwise
  -- create a fresh var (the unification still constrains `r1`'s type).
  implTv <- maybe (fmap TVar freshTV) pure $ Map.lookup lb.ident r2.typ.impl

  let tcs :: Set TypeClass
      tcs = r1.tcs <> r2.tcs

  unify [ImplicitVarTypeOverlap tcs lb.ident implTv r1.typ.body loc] implTv r1.typ.body
  merged <-
    mergeImplMaps
      loc
      [ r1.typ.impl
      , Map.withoutKeys r2.typ.impl $
          Set.singleton lb.ident
      ]
  pure
    InferResult
      { expr = Let lb.letPos lb.varPos (Impl lb.ident) lb.eqPos r1.expr lb.inPos r2.expr
      , typ = ImplType merged r2.typ.body
      , tcs
      }

-- | Infer a type-annotated let binding @let x : T = e1 in e2@. Instantiates
-- the annotation, unifies it with @e1@'s inferred type, and defers the
-- annotation's typeclass constraints for later resolution.
inferLetAnnot :: Location SourcePos -> LetAnnotBinding -> Infer s InferResult
inferLetAnnot loc lb = do
  r1 <- infer lb.rhs
  (annotTcs, ImplType annotImpl annotBody) <- instantiate lb.scheme

  unify
    [AnnotationUnificationFail r1.tcs r1.typ.body annotBody $ blockPosition lb.rhs]
    r1.typ.body
    annotBody

  attachTypeToPosition (elementPosition lb.varPos (Expl lb.ident)) $
    TypeMetadata
      { identExpr = Var () () LocalScope $ Expl lb.ident
      , ty = (annotTcs, ImplType annotImpl annotBody)
      , docs = Nothing
      }

  let binding :: (ExtIdent, TypeMetadata TCScheme)
      binding =
        ( lb.ident
        , TypeMetadata
            { identExpr = Var () () LocalScope $ Expl lb.ident
            , ty = ForallTC [] annotTcs $ ImplType annotImpl annotBody
            , docs = Nothing
            }
        )

  r2 <- inEnv binding $ infer lb.inExpr
  merged <- mergeImplMaps loc [r1.typ.impl, r2.typ.impl, annotImpl]

  -- Defer annotation typeclasses for post-inference resolution
  for_ annotTcs $ deferTC loc

  pure
    InferResult
      { expr =
          LetAnnot lb.letPos lb.varPos lb.ident lb.annotPos lb.scheme lb.eqPos r1.expr lb.inPos r2.expr
      , typ = ImplType merged r2.typ.body
      , tcs = r1.tcs <> r2.tcs <> annotTcs
      }

-- | Infer an infix operator application @e1 `op` e2@. Looks up the operator
-- (must be pinned), decomposes its type into @u1 -> u2 -> u3@, and unifies
-- each component with the operand types.
inferOp :: Location SourcePos -> OpBinding -> Infer s InferResult
inferOp loc ob = do
  r1 <- infer ob.lhs
  r2 <- infer ob.rhs
  meta <- lookupPinnedOp opLoc ob.pin

  let opTcs :: Set TypeClass
      opTcs = fst meta.ty

  (u1, u2, u3) <- opDecompose opLoc opTcs $ snd meta.ty
  tv <- TVar <$> freshTV
  merged <- mergeImplMaps loc [r1.typ.impl, r2.typ.impl]

  let tcs :: Set TypeClass
      tcs = r1.tcs <> r2.tcs

  unify [UnificationFail tcs u1 r1.typ.body $ blockPosition ob.lhs] u1 r1.typ.body
  unify [UnificationFail tcs u2 r2.typ.body $ blockPosition ob.rhs] u2 r2.typ.body
  unify [UnificationFail tcs u3 tv loc] u3 tv

  attachTypeToPosition opLoc $
    meta
      { ty =
          ( opTcs
          , ImplType mempty $ r1.typ.body `TArr` (r2.typ.body `TArr` tv)
          )
      }

  for_ opTcs $ deferTC opLoc

  pure
    InferResult
      { expr = Op r1.expr ob.opPos ob.pin ob.opMeta ob.modNm ob.op r2.expr
      , typ = ImplType merged tv
      , tcs = tcs <> opTcs
      }
  where
    opLoc :: Location SourcePos
    opLoc = mkOpLoc ob.opPos ob.op ob.modNm

-- | Infer a prefix operator application @op e@. Looks up the operator
-- (must be pinned), decomposes its type into @u1 -> u2@, and unifies
-- each component with the operand type.
inferPreOp :: Location SourcePos -> PreOpBinding -> Infer s InferResult
inferPreOp loc pb = do
  r <- infer pb.operand
  meta <- lookupPinnedOp opLoc pb.pin

  let opTcs :: Set TypeClass
      opTcs = fst meta.ty

  (u1, u2) <- preOpDecompose opLoc opTcs $ snd meta.ty
  tv <- TVar <$> freshTV

  unify [UnificationFail r.tcs u1 r.typ.body $ blockPosition pb.operand] u1 r.typ.body
  unify [UnificationFail r.tcs u2 tv loc] u2 tv

  attachTypeToPosition opLoc $
    meta
      { ty = (opTcs, ImplType mempty $ r.typ.body `TArr` tv)
      }

  for_ opTcs $ deferTC opLoc

  pure
    InferResult
      { expr = PreOp pb.opPos pb.pin pb.opLvl pb.modNm pb.op r.expr
      , typ = ImplType r.typ.impl tv
      , tcs = r.tcs <> opTcs
      }
  where
    opLoc :: Location SourcePos
    opLoc = mkOpLoc pb.opPos pb.op pb.modNm

-- | Infer an @if cond then tr else fl@ expression. Unifies the
-- condition with @bool@ and unifies both branches.
inferIf :: Location SourcePos -> IfBinding -> Infer s InferResult
inferIf loc ib = do
  rc <- infer ib.cond
  rt <- infer ib.tr
  rf <- infer ib.fl
  merged <- mergeImplMaps loc [rc.typ.impl, rt.typ.impl, rf.typ.impl]

  let tcs :: Set TypeClass
      tcs = rc.tcs <> rt.tcs <> rf.tcs

  unify [IfConditionMustBeBool tcs rc.typ.body $ blockPosition ib.cond] rc.typ.body typeBool
  unify
    [ IfBranchesMustBeEqType tcs rt.typ.body rf.typ.body (blockPosition ib.tr) $
        blockPosition ib.fl
    ]
    rt.typ.body
    rf.typ.body

  pure
    InferResult
      { expr = If ib.ifPos rc.expr ib.thenPos rt.expr ib.elsePos rf.expr
      , typ = ImplType merged rt.typ.body
      , tcs
      }

-- | Infer a tuple expression @(e1, e2, ...)@. Infers each element,
-- merges implicit maps, and wraps the result types in @TTuple@.
inferTuple ::
  Expr (Pinned VCObjectHash) SourcePos ->
  Location SourcePos ->
  SourcePos ->
  TList (Expr (Pinned VCObjectHash) SourcePos, Maybe SourcePos) ->
  SourcePos ->
  Infer s InferResult
inferTuple expr loc open elems close = do
  results <- traverse (infer . fst) elemList
  merged <- mergeImplMaps loc $ fmap (.typ.impl) results

  let tupTy :: ImplType
      tupTy = ImplType merged . TTuple . tListFromList $ fmap (.typ.body) results

  attachTypeToPosition loc $
    TypeMetadata
      { identExpr = bimap (const ()) (const ()) $ removeComments expr
      , ty = (mempty, tupTy)
      , docs = Nothing
      }
  pure
    InferResult
      { expr =
          flip (Tuple open) close . tListFromList $
            zip (fmap (.expr) results) commas
      , typ = tupTy
      , tcs = foldMap (.tcs) results
      }
  where
    elemList :: [(Expr (Pinned VCObjectHash) SourcePos, Maybe SourcePos)]
    elemList = tListToList elems

    commas :: [Maybe SourcePos]
    commas = fmap snd elemList

-- | Infer an @assert cond in e@ expression. Unifies the condition
-- with @bool@ and returns the body type unchanged.
inferAssert ::
  Location SourcePos ->
  SourcePos ->
  Expr (Pinned VCObjectHash) SourcePos ->
  SourcePos ->
  Expr (Pinned VCObjectHash) SourcePos ->
  Infer s InferResult
inferAssert loc assertPos cond inPos e = do
  rc <- infer cond
  rb <- infer e
  merged <- mergeImplMaps loc [rc.typ.impl, rb.typ.impl]

  let tcs :: Set TypeClass
      tcs = rc.tcs <> rb.tcs

  unify [AssertConditionMustBeBool tcs rc.typ.body $ blockPosition cond] rc.typ.body typeBool

  pure
    InferResult
      { expr = Assert assertPos rc.expr inPos rb.expr
      , typ = ImplType merged rb.typ.body
      , tcs
      }

-- | Infer a @match e { | p1 -> e1 | p2 -> e2 ... }@ expression.
-- Infers the scrutinee, generates pattern constraints, unifies all
-- pattern types with the scrutinee, and unifies all branch body types.
-- Returns a 'CaseResult'; the caller assembles the final 'Case' 'Expr'.
inferCase ::
  forall s.
  Location SourcePos ->
  Expr (Pinned VCObjectHash) SourcePos ->
  NonEmpty CaseBranch ->
  Infer s CaseResult
inferCase loc scrut branches = do
  -- Check for duplicate variable bindings within each pattern
  for_ branches $ void . checkVarOverlap mempty . (.pat)

  r <- infer scrut
  patResults <- for branches $ mkPatConstraint . (.pat)

  -- Register patterns for exhaustiveness checking
  addCasePatterns loc $ toList pats

  let patTys :: NonEmpty InfernoType
      patTys = fmap fst patResults

  -- Infer each branch body with the pattern variables in scope
  bodyResults <-
    for (NonEmpty.zip (fmap snd patResults) branches) $ \(vars, branch) ->
      foldr (inEnv . bindVar) (infer branch.body) vars

  merged <-
    mergeImplMaps loc $
      r.typ.impl : toList (fmap (.typ.impl) bodyResults)

  let tcs :: Set TypeClass
      tcs = r.tcs <> foldMap (.tcs) bodyResults

      mk :: InfernoType -> Pat (Pinned VCObjectHash) SourcePos -> Infer s ()
      mk t p = unify [PatternUnificationFail t r.typ.body p $ blockPosition p] t r.typ.body

  -- All pattern types must be equal to each other
  unifyPairs (toList pats) (toList patTys) $
    \p1 t1 p2 t2 ->
      [PatternsMustBeEqType tcs t1 t2 p1 p2 (blockPosition p1) (blockPosition p2)]

  -- Each pattern type must equal the scrutinee type
  zipWithM_ mk (toList patTys) $ toList pats

  -- All branch body types must be equal
  unifyPairs (toList $ fmap (.body) branches) (toList $ fmap (.typ.body) bodyResults) $ \e1 t1 e2 t2 ->
    [CaseBranchesMustBeEqType tcs t1 t2 (blockPosition e1) (blockPosition e2)]

  let rebuiltBranches :: NonEmpty CaseBranch
      rebuiltBranches =
        NonEmpty.zip branches bodyResults
          <&> \(b, res) -> CaseBranch b.barPos b.pat b.arrPos res.expr

  pure
    CaseResult
      { scrutExpr = r.expr
      , branches = rebuiltBranches
      , typ = ImplType merged (NonEmpty.head bodyResults).typ.body
      , tcs
      }
  where
    bindVar :: (Ident, TypeMetadata TCScheme) -> (ExtIdent, TypeMetadata TCScheme)
    bindVar (Ident x, meta) = (ExtIdent $ Right x, meta)

    pats :: NonEmpty (Pat (Pinned VCObjectHash) SourcePos)
    pats = fmap (.pat) branches

    -- For each distinct pair `(a_i, t_i)` and `(a_j, t_j)` where `i < j`,
    -- call `unify (mkErr a_i t_i a_j t_j) t_i t_j`.
    unifyPairs ::
      forall a.
      [a] ->
      [InfernoType] ->
      (a -> InfernoType -> a -> InfernoType -> [TypeError SourcePos]) ->
      Infer s ()
    unifyPairs as ts mkErr = go $ zip as ts
      where
        go :: [(a, InfernoType)] -> Infer s ()
        go = \case
          [] -> pure ()
          (a1, t1) : rest -> do
            for_ rest $ \(a2, t2) -> unify (mkErr a1 t1 a2 t2) t1 t2
            go rest

    addCasePatterns ::
      Location SourcePos ->
      [Pat (Pinned VCObjectHash) SourcePos] ->
      Infer s ()
    addCasePatterns k ps =
      asks (.refs.patternsToCheck) >>= \ref ->
        liftST . modifySTRef' ref $ ((k, ps) :)

    -- Check that no variable appears more than once in a pattern.
    -- Threads a map of seen variables and their locations through the
    -- recursive walk; throws on duplicate.
    checkVarOverlap ::
      Map Ident (Location SourcePos) ->
      Pat (Pinned VCObjectHash) SourcePos ->
      Infer s (Map Ident (Location SourcePos))
    checkVarOverlap vars pat = case pat of
      PVar _ (Just x) ->
        maybe
          (pure (Map.insert x (blockPosition pat) vars))
          (throwError . pure . VarMultipleOccurrence x (blockPosition pat))
          $ Map.lookup x vars
      POne _ p -> checkVarOverlap vars p
      PArray _ ps _ -> foldM checkVarOverlap vars $ fmap fst ps
      PTuple _ ps _ -> foldM checkVarOverlap vars . fmap fst $ tListToList ps
      PRecord _ ps _ -> foldM checkVarOverlap vars $ fmap snd3 ps
      _ -> pure vars

    -- Infer the type of a pattern and return any variables it binds.
    mkPatConstraint ::
      Pat (Pinned VCObjectHash) SourcePos ->
      Infer s (InfernoType, [(Ident, TypeMetadata TCScheme)])
    mkPatConstraint pat = case pat of
      PVar _ (Just (Ident x)) ->
        (freshTV >>=) . (. TVar) $ \tv -> do
          attachTypeToPosition patLoc $
            TypeMetadata
              { identExpr = Var () () LocalScope . Expl . ExtIdent $ Right x
              , ty = (mempty, ImplType mempty tv)
              , docs = Nothing
              }
          pure
            ( tv
            ,
              [
                ( Ident x
                , TypeMetadata
                    { identExpr = Var () () LocalScope . Expl . ExtIdent $ Right x
                    , ty = ForallTC mempty mempty $ ImplType mempty tv
                    , docs = Nothing
                    }
                )
              ]
            )
      PVar _ Nothing ->
        (freshTV >>=) . (. TVar) $ \tv -> do
          attachTypeToPosition patLoc $
            TypeMetadata
              { identExpr = patternToExpr $ bimap (const ()) (const ()) pat
              , ty = (mempty, ImplType mempty tv)
              , docs = Nothing
              }
          pure (tv, mempty)
      PEnum _ pin sc i ->
        lookupPinned patLoc pin (EnumNamespace i) >>= \meta -> do
          attachTypeToPosition patLoc $ meta{identExpr = Enum () () sc i}
          pure ((snd meta.ty).body, mempty)
      PLit _ l -> do
        attachTypeToPosition patLoc $
          TypeMetadata
            { identExpr = Lit () l
            , ty = (mempty, ImplType mempty t)
            , docs = Nothing
            }
        pure (t, mempty)
        where
          t :: InfernoType
          t = case l of
            LInt _ -> typeInt
            LDouble _ -> typeDouble
            LHex _ -> typeWord64
            LText _ -> typeText
      POne _ p -> do
        (t, vars) <- mkPatConstraint p
        meta <- lookupEnv patLoc $ Left oneHash
        attachTypeToPosition patLoc $
          meta
            { ty = (mempty, ImplType mempty . TArr t $ TOptional t)
            }
        pure (TOptional t, vars)
      PEmpty _ ->
        lookupEnv patLoc (Left emptyHash) >>= \meta -> do
          attachTypeToPosition patLoc meta
          pure ((snd meta.ty).body, mempty)
      PArray _ [] _ -> do
        (freshTV >>=) . (. TArray . TVar) $ \t -> do
          attachTypeToPosition patLoc $
            TypeMetadata
              { identExpr = patternToExpr $ bimap (const ()) (const ()) pat
              , ty = (mempty, ImplType mempty t)
              , docs = Nothing
              }
          pure (t, mempty)
      PArray _ ((p, _) : ps) _ -> do
        (t, vars1) <- mkPatConstraint p
        vars2 <- inferArrayPatElems t ps

        attachTypeToPosition patLoc $
          TypeMetadata
            { identExpr = patternToExpr $ bimap (const ()) (const ()) pat
            , ty = (mempty, ImplType mempty $ TArray t)
            , docs = Nothing
            }
        pure (TArray t, vars1 <> vars2)
      PTuple _ ps _ ->
        traverse (mkPatConstraint . fst) (tListToList ps) >>= \results -> do
          let tupTy :: InfernoType
              tupTy = TTuple . tListFromList $ fmap fst results

              vars :: [(Ident, TypeMetadata TCScheme)]
              vars = concatMap snd results
          attachTypeToPosition patLoc $
            TypeMetadata
              { identExpr = patternToExpr $ bimap (const ()) (const ()) pat
              , ty = (mempty, ImplType mempty tupTy)
              , docs = Nothing
              }
          pure (tupTy, vars)
      PRecord _ fs _ -> do
        checkDuplicateFields patLoc fs
        results <- traverse (mkPatConstraint . snd3) fs

        let fields :: Map Ident InfernoType
            fields =
              Map.fromList $
                zip (fmap fst3 fs) $
                  fmap fst results

            vars :: [(Ident, TypeMetadata TCScheme)]
            vars = concatMap snd results

            recTy :: InfernoType
            recTy = TRecord fields RowAbsent

        attachTypeToPosition patLoc $
          TypeMetadata
            { identExpr = patternToExpr $ bimap (const ()) (const ()) pat
            , ty = (mempty, ImplType mempty recTy)
            , docs = Nothing
            }
        pure (recTy, vars)
      PCommentAbove _ p -> mkPatConstraint p
      PCommentAfter p _ -> mkPatConstraint p
      PCommentBelow p _ -> mkPatConstraint p
      where
        patLoc :: Location SourcePos
        patLoc = blockPosition pat

    -- Infer remaining elements of an array pattern, unifying each with the
    -- first element's type.
    inferArrayPatElems ::
      InfernoType ->
      [(Pat (Pinned VCObjectHash) SourcePos, Maybe SourcePos)] ->
      Infer s [(Ident, TypeMetadata TCScheme)]
    inferArrayPatElems = \cases
      _ [] -> pure []
      t ((p, _) : rest) -> do
        (tp, vars1) <- mkPatConstraint p
        unify [UnificationFail mempty t tp $ blockPosition p] t tp
        vars2 <- inferArrayPatElems t rest
        pure $ vars1 <> vars2

-- | Infer @Some e@. Infers the inner expression, wraps its type in
-- @TOptional@, and attaches the metadata from the builtin `oneHash`.
inferOne ::
  Location SourcePos ->
  SourcePos ->
  Expr (Pinned VCObjectHash) SourcePos ->
  Infer s InferResult
inferOne loc pos e = do
  r <- infer e
  meta <- lookupEnv loc $ Left oneHash

  let resTy :: ImplType
      resTy = ImplType r.typ.impl $ TOptional r.typ.body

  attachTypeToPosition
    loc
    meta
      { ty = (mempty, resTy)
      }
  pure
    InferResult
      { expr = One pos r.expr
      , typ = resTy
      , tcs = r.tcs
      }

-- | Infer @None@. Looks up the builtin `emptyHash` to get the polymorphic
-- @optional a@ type, instantiates it, and attaches the metadata.
inferEmpty ::
  Expr (Pinned VCObjectHash) SourcePos ->
  Location SourcePos ->
  Infer s InferResult
inferEmpty expr loc =
  lookupEnv loc (Left emptyHash) >>= \meta -> do
    attachTypeToPosition loc meta
    pure
      InferResult
        { expr
        , typ = snd meta.ty
        , tcs = mempty
        }

-- | Infer @let module NewName = OldName in e@. Temporarily aliases an existing
-- module under a new name while inferring the body expression.
inferRenameModule :: RenameModuleBinding -> Infer s InferResult
inferRenameModule rb = do
  ref <- asks (.refs.modules)
  mods <- liftST $ readSTRef ref
  when (rb.newNm `Map.member` mods) $
    throwError [ModuleNameTaken rb.newNm $ elementPosition rb.namePos rb.newNm]
  Map.lookup rb.oldNm mods & \case
    Nothing -> throwError [ModuleDoesNotExist rb.oldNm (rb.oldPos, rb.inPos)]
    Just oldMod -> do
      liftST . writeSTRef ref $ Map.insert rb.newNm oldMod mods
      r <- infer rb.body
      -- Restore the original module map (remove the alias)
      liftST . modifySTRef' ref $ Map.delete rb.newNm
      pure
        InferResult
          { expr = RenameModule rb.namePos rb.newNm rb.oldPos rb.oldNm rb.inPos r.expr
          , typ = r.typ
          , tcs = r.tcs
          }

-- | Infer @open ModuleName (imports) in e@. Verifies the module exists
-- and infers the body (imports are resolved during parsing/pinning).
inferOpenModule :: OpenModuleBinding -> Infer s InferResult
inferOpenModule ob = do
  mods <- liftST . readSTRef =<< asks (.refs.modules)
  Map.lookup ob.modNm mods & \case
    Nothing ->
      throwError [ModuleDoesNotExist ob.modNm $ elementPosition ob.openPos ob.modNm]
    Just _ -> do
      infer ob.body <&> \r ->
        InferResult
          { expr =
              OpenModule ob.openPos ob.pin ob.modNm ob.imports ob.inPos r.expr
          , typ = r.typ
          , tcs = r.tcs
          }

-- | Infer a comment or bracketed wrapper. These are structurally transparent;
-- the inner expression is inferred and the wrapper is preserved.
inferCommentAbove ::
  Comment SourcePos -> Expr (Pinned VCObjectHash) SourcePos -> Infer s InferResult
inferCommentAbove c e =
  infer e <&> \r -> r{expr = CommentAbove c r.expr}

inferCommentAfter ::
  Expr (Pinned VCObjectHash) SourcePos -> Comment SourcePos -> Infer s InferResult
inferCommentAfter e c =
  infer e <&> \r -> r{expr = CommentAfter r.expr c}

inferCommentBelow ::
  Expr (Pinned VCObjectHash) SourcePos -> Comment SourcePos -> Infer s InferResult
inferCommentBelow e c =
  infer e <&> \r -> r{expr = CommentBelow r.expr c}

inferBracketed ::
  SourcePos -> Expr (Pinned VCObjectHash) SourcePos -> SourcePos -> Infer s InferResult
inferBracketed p1 e p2 =
  infer e <&> \r -> r{expr = Bracketed p1 r.expr p2}

-- | Infer a @TypeRep@. Produces @TRep t@ directly with no constraints.
inferTypeRep ::
  Expr (Pinned VCObjectHash) SourcePos -> InfernoType -> Infer s InferResult
inferTypeRep expr t =
  pure
    InferResult
      { expr
      , typ = ImplType mempty $ TRep t
      , tcs = mempty
      }

-- | Compute the source location span for an operator, accounting for
-- an optional module prefix (e.g. @Module.+@).
mkOpLoc :: SourcePos -> Ident -> Scoped ModuleName -> Location SourcePos
mkOpLoc pos op modNm =
  (pos, incSourceCol ePos . fromScoped 0 $ fmap modLen modNm)
  where
    ePos :: SourcePos
    ePos = snd $ elementPosition pos op

    modLen :: ModuleName -> Int
    modLen (ModuleName nm) = Text.length nm + 1

-- | Look up a pinned operator in the environment. Operators must always
-- be pinned; uses @error@ for the impossible unpinned case.
lookupPinnedOp ::
  Location SourcePos ->
  Pinned VCObjectHash ->
  Infer s (TypeMetadata (Set TypeClass, ImplType))
lookupPinnedOp loc pin =
  lookupEnv loc
    . Left
    . fromMaybe (error "internal error: operators must always be pinned")
    $ pinnedToMaybe pin

-- | Decompose a binary operator type @a -> b -> c@ into its three components.
-- If the type doesn't match, creates fresh vars and unifies, producing a
-- proper @ExpectedFunction@ error on failure.
opDecompose ::
  Location SourcePos ->
  Set TypeClass ->
  ImplType ->
  Infer s (InfernoType, InfernoType, InfernoType)
opDecompose opLoc tcs = \case
  ImplType _ (t1 `TArr` (t2 `TArr` t3)) -> pure (t1, t2, t3)
  ImplType _ t -> do
    a <- TVar <$> freshTV
    b <- TVar <$> freshTV
    c <- TVar <$> freshTV
    let expected :: InfernoType
        expected = a `TArr` (b `TArr` c)
    unify [ExpectedFunction tcs expected t opLoc] t expected
    pure (a, b, c)

-- | Decompose a prefix operator type @a -> b@ into its two components.
-- If the type doesn't match, creates fresh vars and unifies, producing a
-- proper @ExpectedFunction@ error on failure.
preOpDecompose ::
  Location SourcePos ->
  Set TypeClass ->
  ImplType ->
  Infer s (InfernoType, InfernoType)
preOpDecompose opLoc tcs = \case
  ImplType _ (t1 `TArr` t2) -> pure (t1, t2)
  ImplType _ t -> do
    a <- TVar <$> freshTV
    b <- TVar <$> freshTV
    let expected :: InfernoType
        expected = a `TArr` b
    unify [ExpectedFunction tcs expected t opLoc] t expected
    pure (a, b)

-------------------------------------------------------------------------------
-- Stubs for Later Phases
-------------------------------------------------------------------------------

-- Given a map of implicit types containing `rep of <ty>` variables, and an expression `e`
-- we want to either substitute any implicit variable `?var$n : rep of <ty>` with a `RuntimeRep <ty>`,
-- provided that `<ty>` contains no free variables
-- otherwise we want to create a closure `fun var$n -> e[var$n/?var$n]`
closeOverTypeReps ::
  Map ExtIdent InfernoType ->
  Expr (Pinned VCObjectHash) SourcePos ->
  (Maybe TypeClass, Map ExtIdent InfernoType, Expr (Pinned VCObjectHash) SourcePos)
closeOverTypeReps = undefined

-- | Solve for the top level type of an expression in a given environment
inferExpr ::
  Map ModuleName (PinnedModule m) ->
  Expr (Pinned VCObjectHash) SourcePos ->
  Either
    [TypeError SourcePos]
    ( Expr (Pinned VCObjectHash) SourcePos
    , TCScheme
    , Map (Location SourcePos) (TypeMetadata TCScheme)
    )
inferExpr = undefined

-- | Given a type signature and some concrete assignment of types (assumes
-- @inputTys@ and @outputTy@ have no free variables) this function computes
-- the runtime reps
inferTypeReps ::
  Set TypeClass ->
  TCScheme ->
  [InfernoType] ->
  InfernoType ->
  Either [TypeError SourcePos] [InfernoType]
inferTypeReps = undefined

inferPossibleTypes ::
  Set TypeClass ->
  TCScheme ->
  [Maybe InfernoType] ->
  Maybe InfernoType ->
  Either [TypeError SourcePos] ([[InfernoType]], [InfernoType])
inferPossibleTypes = undefined

findTypeClassWitnesses ::
  Set TypeClass ->
  Maybe Int ->
  Set TypeClass ->
  Set TV ->
  [Subst]
findTypeClassWitnesses = undefined
