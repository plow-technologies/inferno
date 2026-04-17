{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
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
import Control.Monad (join, when, (<=<))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Extra (whenM, zipWithM_)
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (lift)
import Data.Bifunctor (bimap)
import Data.Foldable (foldl', for_, toList) -- foldl': see CLAUDE.md note about lower GHC versions
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.List (find)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Sequence (Seq, (<|))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Traversable (for)
import Data.Tuple.Extra (fst3, snd3, thd3)
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
      ( DuplicateRecordField,
        ImplicitVarTypeOverlap,
        InfiniteType,
        UnboundExtIdent,
        UnboundNameInNamespace,
        UnificationFail,
        VarMultipleOccurrence
      ),
  )
import Inferno.Types.Module (Module, PinnedModule)
import Inferno.Types.Syntax
  ( BlockUtils (blockPosition, removeComments),
    ElementPosition (elementPosition),
    Expr
      ( App,
        Array,
        ArrayComp,
        InterpolatedString,
        Lit,
        Record,
        Var
      ),
    ExtIdent (ExtIdent),
    Ident (Ident),
    ImplExpl (Expl, Impl),
    Lit (LDouble, LHex, LInt, LText),
    ModuleName,
    Pat,
    RestOfRecord (RowAbsent, RowVar),
    Scoped (LocalScope),
    SomeIStr,
    fromEitherList,
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

-- | A single branch of a @match@ expression
data CaseBranch = CaseBranch
  { barPos :: SourcePos
  , pat :: Pat (Pinned VCObjectHash) SourcePos
  , arrPos :: SourcePos
  , body :: Expr (Pinned VCObjectHash) SourcePos
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
type UFStore s = STRef s (Vector.Mutable.STVector s UFContent)

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
withStore :: (Vector.Mutable.STVector s UFContent -> ST s a) -> Infer s a
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
    UFLink _ -> pure (0, Nothing) -- unreachable after ufFind

-- | Union two type variables by rank. If either root already has a
-- resolved type, the other root inherits it. If both have resolved
-- types, they are unified for consistency.
ufUnion :: TV -> TV -> Infer s ()
ufUnion a b =
  withFind a $ \ra@(TV ri) -> withFind b $ \rb@(TV rj) ->
    when (ra /= rb) $ do
      (rankA, mA) <- readRoot ra
      (rankB, mB) <- readRoot rb
      merged <- case (mA, mB) of
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
  asks (.refs.typeMap) >>= \ref ->
    liftST . modifySTRef' ref $ Map.insert k meta

-- | Merge a list of implicit maps. When two maps share a key, 'unify' the
-- types immediately (eager unification replaces the old constraint-based
-- approach).
mergeImplMaps ::
  forall s.
  Location SourcePos ->
  [Map ExtIdent InfernoType] ->
  Infer s (Map ExtIdent InfernoType)
mergeImplMaps loc =
  flip foldl' (pure Map.empty) $
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
    go _ [] = pure ()
    go seen ((f, _, _) : rest)
      | Set.member f seen = throwError [DuplicateRecordField f loc]
      | otherwise = go (Set.insert f seen) rest

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
              (\hsh -> UnboundNameInNamespace LocalScope (Left hsh) loc)
              (\i -> UnboundExtIdent LocalScope i loc)
              x
          ]
      Just meta -> do
        iTy <- instantiate meta.ty
        pure meta{ty = iTy}

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
instantiate (ForallTC as tcs t) = do
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
    tail1 <- expandRowVar err s1.ror s1.new
    tail2 <- expandRowVar err s2.ror s2.new
    unifyRowVars err tail1 tail2
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
              , tyClasses = Set.empty
              , patternsToCheck = patsRef
              , deferred = defRef
              }

          ctx :: InferCtx s
          ctx = InferCtx{env = mempty, refs = refs}

          doUnify :: Infer s ()
          doUnify =
            unifyRecordFields
              err
              RecordSide{fields = Map.toAscList fs1, ror = ror1, new = mempty}
              RecordSide{fields = Map.toAscList fs2, ror = ror2, new = mempty}
              mempty

      runExceptT (runReaderT doUnify ctx) >>= \case
        Left errs -> pure $ Left errs
        Right () -> do
          n <- readSTRef cRef
          v <- readSTRef stRef
          freezeUFToSubst n v

-- | Freeze the UF store into a 'Subst'. For each TV @0..n-1@, fully
-- resolve its type (transitively resolving all inner @TVar@s) and add
-- the mapping if it differs from the identity.
freezeUFToSubst ::
  forall s.
  Int ->
  Vector.Mutable.STVector s UFContent ->
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
      , ty = (Set.singleton tyCls, ImplType Map.empty tv)
      , docs = Nothing
      }
  pure
    InferResult
      { expr = App expr (Var pos Local LocalScope $ Impl i)
      , typ = ImplType (Map.singleton i $ TRep tv) tv
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
      , ty = (Set.empty, ImplType Map.empty t)
      , docs = Nothing
      }
  pure InferResult{expr = expr, typ = ImplType Map.empty t, tcs = Set.empty}
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
  case find isRepTC $ Set.toList tcs of
    Just (TypeClass _ repTys) -> do
      traverse mkRepImpl repTys <&> \repImpls ->
        InferResult
          { expr =
              foldl' App expr $
                fmap (Var pos Local LocalScope . Impl . fst) repImpls
          , typ = ImplType (iType.impl `Map.union` Map.fromList repImpls) iType.body
          , tcs = Set.filter (not . isRepTC) tcs
          }
    Nothing ->
      pure
        InferResult
          { expr = expr
          , typ = iType
          , tcs = Set.filter (not . isRepTC) tcs
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
      , ty = (Set.empty, implTy)
      , docs = Nothing
      }
  pure InferResult{expr = expr, typ = implTy, tcs = Set.empty}

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
        { expr = expr
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
    pure InferResult{expr = expr, typ = snd meta.ty, tcs = Set.empty}

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
      , ty = (Set.empty, ImplType Map.empty typeText)
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
      Left str -> pure (Left str, Map.empty, Set.empty)
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
      , ty = (Set.empty, recTy)
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
      { expr = expr
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
      , ty = (Set.empty, arrTy)
      , docs = Nothing
      }
  pure
    InferResult
      { expr = Array open (zipWith (\(_, p) r -> (r.expr, p)) elems results) close
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
        r <$
          unify [UnificationFail r.tcs tv r.typ.body $ blockPosition e] tv r.typ.body

-- | Infer an array comprehension @[body | x <- gen, ... if cond]@.
-- Each selector binds a variable into scope for subsequent selectors,
-- the body, and the optional condition.
inferArrayComp ::
  Expr (Pinned VCObjectHash) SourcePos ->
  Location SourcePos ->
  SourcePos ->
  Expr (Pinned VCObjectHash) SourcePos ->
  SourcePos ->
  NonEmpty Selector ->
  Maybe (SourcePos, Expr (Pinned VCObjectHash) SourcePos) ->
  SourcePos ->
  Infer s InferResult
inferArrayComp _expr loc open body pipe (toList -> sels) cond close = do
  checkVarOverlap sels mempty

  cr <- processSels sels
  merged <- mergeImplMaps loc $ cr.bodyResult.typ.impl : cr.condImpl : cr.selImpls
  rebuiltSels <-
    toList cr.rebuiltSels & \case
      h : t -> pure $ fmap fromSelector (h :| t)
      -- Not possible in practice
      [] -> throwError []

  pure
    InferResult
      { expr = ArrayComp open cr.bodyResult.expr pipe rebuiltSels cr.condExpr close
      , typ = ImplType merged . TArray $ cr.bodyResult.typ.body
      , tcs = cr.bodyResult.tcs <> cr.condTcs <> cr.selTcs
      }
  where
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
        rBody <- infer body
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
    inferCond = case cond of
      Nothing -> pure (Nothing, Map.empty, Set.empty)
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

-------------------------------------------------------------------------------
-- Stubs for Later Phases
-------------------------------------------------------------------------------

infer :: Expr (Pinned VCObjectHash) SourcePos -> forall s. Infer s InferResult
infer = undefined

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
