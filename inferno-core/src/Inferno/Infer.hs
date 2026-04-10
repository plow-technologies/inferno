{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Inferno.Infer
  ( Constraint,
    TypeError (..),
    Subst (..),
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
import Control.Monad (when, (<=<))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Extra (whenM, zipWithM_)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (lift)
import Data.Foldable (for_, foldl', toList) -- foldl': see CLAUDE.md note about lower GHC versions
import Data.Function ((&))
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Set as Set
import qualified Data.Vector.Mutable as MVector
import GHC.Records (HasField (getField))
import Inferno.Infer.Env (Env, TypeMetadata, closeOver, closeOverType)
import Inferno.Infer.Error (TypeError (InfiniteType))
import Inferno.Types.Module (Module, PinnedModule)
import Inferno.Types.Syntax
  ( Expr,
    ExtIdent,
    Ident,
    ModuleName,
    Pat,
    RestOfRecord (RowAbsent, RowVar),
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
    TCScheme,
    TV (TV, unTV),
    TypeClass (TypeClass),
  )
import Inferno.Types.VersionControl (Pinned, VCObjectHash)
import Prettyprinter (Pretty (pretty), (<+>))
import Text.Megaparsec (SourcePos)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type Location a = (a, a)

-- | Type equality or typeclass constraint (kept for API compatibility)
type Constraint =
  Either
    ( InfernoType
    , InfernoType
    , [TypeError SourcePos]
    )
    ( Location SourcePos
    , TypeClass
    )

-- | Result of inferring a sub-expression: the elaborated expression,
-- its implicit type, and the generated typeclasses (for error context).
data InferResult = InferResult
  { expr :: Expr (Pinned VCObjectHash) SourcePos
  , typ :: ImplType
  , tcs :: !(Set.Set TypeClass)
  }

-- | A single branch of a @match@ expression
data CaseBranch = CaseBranch
  { barPos :: SourcePos
  , pat :: Pat (Pinned VCObjectHash) SourcePos
  , arrPos :: SourcePos
  , body :: Expr (Pinned VCObjectHash) SourcePos
  }

-- | One side of a record-field merge: remaining sorted fields,
-- the row variable tail, and accumulated new fields.
data RecordSide = RecordSide
  { fields :: ![(Ident, InfernoType)]
  , ror :: !RestOfRecord
  , new :: ![(Ident, InfernoType)]
  }

-- | Virtual record fields for @ImplType@
instance HasField "impl" ImplType (Map.Map ExtIdent InfernoType) where
  getField (ImplType m _) = m

instance HasField "body" ImplType InfernoType where
  getField (ImplType _ t) = t

instance Substitutable Constraint where
  apply s = \case
    Left (t1, t2, es) -> Left (apply s t1, apply s t2, es)
    Right (loc, tc) -> Right (loc, apply s tc)
  ftv = \case
    Left (t1, t2, _) -> ftv t1 `Set.union` ftv t2
    Right (_, tc) -> ftv tc

instance Pretty Constraint where
  pretty = \case
    Left (t1, t2, _) -> pretty t1 <+> "~" <+> pretty t2
    Right (_, tc) -> pretty tc

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
type UFStore s = STRef s (MVector.STVector s UFContent)

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
          (Map.Map (Location SourcePos) (TypeMetadata (Set.Set TypeClass, ImplType)))
       )
  , modules :: !(STRef s (Map.Map ModuleName (Module ())))
  , tyClasses :: !(Set.Set TypeClass)
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
withStore :: (MVector.STVector s UFContent -> ST s a) -> Infer s a
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
            cap0 = MVector.length v
            cap1 = max (cap0 * 2) (i + 1)
         in when (i >= cap0) $
              writeSTRef storeRef =<< MVector.grow v (cap1 - cap0)

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
  withStore $ \v -> MVector.write v n $ UFRoot 0 Nothing
  pure n

-- | Chase 'UFLink' pointers to the root, applying path compression.
ufFind :: TV -> Infer s TV
ufFind tv@(TV i) =
  withStore (`MVector.read` i) >>= \case
    UFRoot _ _ -> pure tv
    UFLink parent -> do
      root <- ufFind parent
      -- Path compression
      when (root /= parent) $ withStore $ \v -> MVector.write v i $ UFLink root
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
  withStore (`MVector.read` ri) >>= \case
    UFRoot rank mt -> pure (rank, mt)
    UFLink _ -> pure (0, Nothing) -- unreachable after ufFind

-- | Union two type variables by rank. If either root already has a
-- resolved type, the other root inherits it (and we unify if both have one).
ufUnion :: TV -> TV -> Infer s ()
ufUnion a b =
  withFind a $ \ra@(TV ri) -> withFind b $ \rb@(TV rj) ->
    when (ra /= rb) $ do
      (rankA, mA) <- readRoot ra
      (rankB, mB) <- readRoot rb
      let merged :: Maybe InfernoType
          merged = mA <|> mB
      withStore $ \v ->
        case compare rankA rankB of
          LT -> do
            MVector.write v ri $ UFLink rb
            MVector.write v rj $ UFRoot rankB merged
          GT -> do
            MVector.write v rj $ UFLink ra
            MVector.write v ri $ UFRoot rankA merged
          EQ -> do
            MVector.write v rj $ UFLink ra
            MVector.write v ri $ UFRoot (rankA + 1) merged

-- | Bind a root type variable to a concrete (non-'TVar') type.
ufBind :: TV -> InfernoType -> Infer s ()
ufBind tv t = withFind tv $ \root@(TV ri) ->
  (readRoot root >>=) . (. fst) $ \rank ->
    withStore $ \v -> MVector.write v ri . UFRoot rank $ Just t

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
    -- Zonk a record's row variable; if it resolves to another `TRecord`,
    -- merge the fields and keep going.
    zonkRecord :: Map.Map Ident InfernoType -> RestOfRecord -> Infer s InfernoType
    zonkRecord fs = \case
      RowAbsent -> pure $ TRecord fs RowAbsent
      RowVar tv ->
        ufProbe tv >>= \case
          TVar tv' -> pure . TRecord fs $ RowVar tv'
          TRecord fs' ror' ->
            flip zonkRecord ror' $ Map.union fs fs'
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
-- Core Unification
-------------------------------------------------------------------------------

-- | Unify two types eagerly using the union-find. The error list is used when
-- unification fails.
unify :: [TypeError SourcePos] -> InfernoType -> InfernoType -> Infer s ()
unify err t1 t2 = do
  t1' <- zonk t1
  t2' <- zonk t2
  go t1' t2'
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
        unifyRecordFields err
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
    ror1' <- expandRowVar err s1.ror s1.new
    ror2' <- expandRowVar err s2.ror s2.new
    unifyRowVars err ror1' ror2'
  -- Left exhausted, right has fields remaining
  s1@RecordSide{fields = []} s2@RecordSide{fields = (f2, t2) : ts2} pairs ->
    (freshTV >>=) . (. TVar) $ \tv ->
      unifyRecordFields err
        s1{new = (f2, tv) : s1.new}
        s2{fields = ts2}
        $ (tv, t2) : pairs
  -- Right exhausted, left has fields remaining
  s1@RecordSide{fields = (f1, t1) : ts1} s2@RecordSide{fields = []} pairs ->
    (freshTV >>=) . (. TVar) $ \tv ->
      unifyRecordFields err
        s1{fields = ts1}
        s2{new = (f1, tv) : s2.new}
        $ (tv, t1) : pairs
  -- Both have fields; compare the smallest field names
  s1@RecordSide{fields = (f1, t1) : ts1} s2@RecordSide{fields = (f2, t2) : ts2} pairs
    | f1 == f2 ->
        unifyRecordFields err s1{fields = ts1} s2{fields = ts2} $ (t1, t2) : pairs
    | f1 > f2 ->
        (freshTV >>=) . (. TVar) $ \tv ->
          unifyRecordFields err
            s1{new = (f2, tv) : s1.new}
            s2{fields = ts2}
            $ (tv, t2) : pairs
    | otherwise ->
        (freshTV >>=) . (. TVar) $ \tv ->
          unifyRecordFields err
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
    freshTV >>= \tv' ->
      -- Bind the old row var to a record of the new fields + fresh tail
      ufBind tv (TRecord (Map.fromDescList newFields) (RowVar tv'))
        $> RowVar tv'

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
  (Map.Map Ident InfernoType, RestOfRecord) ->
  (Map.Map Ident InfernoType, RestOfRecord) ->
  Either [TypeError SourcePos] Subst
unifyRecords err (fs1, ror1) (fs2, ror2) = runST go
  where
    -- Compute the starting counter from the max TV in the inputs
    maxTV :: Int
    maxTV = maybe 0 ((+1) . unTV) . Set.lookupMax $
      ftv (TRecord fs1 ror1) `Set.union` ftv (TRecord fs2 ror2)

    go :: forall s. ST s (Either [TypeError SourcePos] Subst)
    go = do
      cRef <- newSTRef maxTV
      vecInit <- MVector.replicate (max 128 (maxTV * 2)) $ UFRoot 0 Nothing
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
            unifyRecordFields err
              RecordSide{fields = Map.toAscList fs1, ror = ror1, new = mempty}
              RecordSide{fields = Map.toAscList fs2, ror = ror2, new = mempty}
              mempty

      runExceptT (runReaderT doUnify ctx) >>= \case
        Left errs -> pure $ Left errs
        Right () -> do
          n <- readSTRef cRef
          v <- readSTRef stRef
          freezeUFToSubst n v

-- | Freeze the UF store into a 'Subst'. For each TV @0..n-1@, if it
-- resolves to something other than itself, add the mapping.
freezeUFToSubst ::
  forall s.
  Int ->
  MVector.STVector s UFContent ->
  ST s (Either [TypeError SourcePos] Subst)
freezeUFToSubst n v =
  Right . Subst . Map.fromList <$> go 0
  where
    go :: Int -> ST s [(TV, InfernoType)]
    go i
      | i >= n = pure mempty
      | otherwise = do
          t <- resolve i
          rest <- go $ i + 1
          t & \case
            TVar tv'
              | tv' == TV i -> pure rest
            _ -> pure $ (TV i, t) : rest

    -- Chase links and return the resolved type
    resolve :: Int -> ST s InfernoType
    resolve i =
      MVector.read v i >>= \case
        UFLink (TV j) -> resolve j
        UFRoot _ (Just t) -> pure t
        UFRoot _ Nothing -> pure . TVar $ TV i

-------------------------------------------------------------------------------
-- Stubs for Later Phases
-------------------------------------------------------------------------------

-- Given a map of implicit types containing `rep of <ty>` variables, and an expression `e`
-- we want to either substitute any implicit variable `?var$n : rep of <ty>` with a `RuntimeRep <ty>`,
-- provided that `<ty>` contains no free variables
-- otherwise we want to create a closure `fun var$n -> e[var$n/?var$n]`
closeOverTypeReps ::
  Map.Map ExtIdent InfernoType ->
  Expr (Pinned VCObjectHash) SourcePos ->
  (Maybe TypeClass, Map.Map ExtIdent InfernoType, Expr (Pinned VCObjectHash) SourcePos)
closeOverTypeReps = undefined

-- | Solve for the top level type of an expression in a given environment
inferExpr ::
  Map.Map ModuleName (PinnedModule m) ->
  Expr (Pinned VCObjectHash) SourcePos ->
  Either
    [TypeError SourcePos]
    ( Expr (Pinned VCObjectHash) SourcePos
    , TCScheme
    , Map.Map (Location SourcePos) (TypeMetadata TCScheme)
    )
inferExpr = undefined

-- | Given a type signature and some concrete assignment of types (assumes
-- @inputTys@ and @outputTy@ have no free variables) this function computes
-- the runtime reps
inferTypeReps ::
  Set.Set TypeClass ->
  TCScheme ->
  [InfernoType] ->
  InfernoType ->
  Either [TypeError SourcePos] [InfernoType]
inferTypeReps = undefined

inferPossibleTypes ::
  Set.Set TypeClass ->
  TCScheme ->
  [Maybe InfernoType] ->
  Maybe InfernoType ->
  Either [TypeError SourcePos] ([[InfernoType]], [InfernoType])
inferPossibleTypes = undefined

infer :: Expr (Pinned VCObjectHash) SourcePos -> forall s. Infer s InferResult
infer = undefined

{-# NOINLINE findTypeClassWitnesses #-}
findTypeClassWitnesses ::
  Set.Set TypeClass ->
  Maybe Int ->
  Set.Set TypeClass ->
  Set.Set TV ->
  [Subst]
findTypeClassWitnesses = undefined
