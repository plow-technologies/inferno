{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Union-find store, unification, and zonking primitives for type inference.
module Inferno.Infer.UF
  ( -- * Union-Find Cell
    UFContent (UFRoot, UFLink),

    -- * Monad Stack
    Location,
    UFStore,
    Infer,
    InferCtx (InferCtx, env, refs),
    InferRefs
      ( InferRefs,
        count,
        ufStore,
        typeMap,
        modules,
        tyClasses,
        patternsToCheck,
        deferred
      ),
    liftST,
    withStore,

    -- * UF Primitives
    ensureCapacity,
    freshTV,
    freshTVRaw,
    ufFind,
    withFind,
    ufProbe,
    readRoot,
    ufUnion,
    ufBind,

    -- * Zonking
    zonk,
    zonkImplType,
    zonkTC,

    -- * Occurs Check
    occursIn,

    -- * Core Unification
    unify,
    RecordSide (RecordSide, fields, ror, new),
    unifyRecordFields,
    expandRowVar,
    unifyRowVars,

    -- * Initialization
    initRefs,

    -- * Running
    runInfer,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (join, when, (<=<))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Extra (zipWithM_)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.ST (ST)
import Control.Monad.Trans (lift)
import Data.Foldable (for_, toList)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as Vector.Mutable
import Inferno.Infer.Env (Env)
import Inferno.Infer.Error (TypeError (InfiniteType))
import Inferno.Types.Module (Module (moduleObjects), PinnedModule)
import Inferno.Types.Syntax
  ( Ident,
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
    Substitutable (ftv),
    TV (TV),
    TypeClass (TypeClass),
    TypeMetadata,
  )
import Inferno.Types.VersionControl (Pinned, VCObjectHash)
import Text.Megaparsec (SourcePos)

-- | Create a self-contained inference context with a given starting TV
-- counter and typeclass set, then run the provided action. The action
-- receives the @deferred@ ref for direct manipulation.
runInfer ::
  forall s a.
  Int ->
  Set TypeClass ->
  (STRef s [(Location SourcePos, TypeClass)] -> Infer s a) ->
  ST s (Either [TypeError SourcePos] a)
runInfer startTV tyClasses k = do
  count <- newSTRef startTV
  v <- flip Vector.Mutable.replicate (UFRoot 0 Nothing) . max 128 $ startTV * 2
  ufStore <- newSTRef v
  typeMap <- newSTRef mempty
  modules <- newSTRef mempty
  patternsToCheck <- newSTRef mempty
  deferred <- newSTRef mempty

  let refs :: InferRefs s
      refs =
        InferRefs
          { count
          , ufStore
          , typeMap
          , modules
          , tyClasses
          , patternsToCheck
          , deferred
          }

      env :: Env
      env = mempty
  runExceptT . flip runReaderT InferCtx{env, refs} $ k deferred

type Location a = (a, a)

-- | A single cell in the union-find store.
-- @UFRoot@ is the root of a set; it carries a rank (for union-by-rank)
-- and an optional resolved (non-@TVar@) type.
-- @UFLink@ redirects to another type variable.
data UFContent
  = UFRoot !Int !(Maybe InfernoType)
  | UFLink !TV

-- | Mutable union-find store, indexed by @unTV :: Int@.
-- Backed by a growable @MVector@ in @ST@.
type UFStore s = STRef s (STVector s UFContent)

-- | The inference monad: @ReaderT@ over @ExceptT@ over @ST@.
-- All mutable state lives in @STRef@s.
type Infer s a = ReaderT (InferCtx s) (ExceptT [TypeError SourcePos] (ST s)) a

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
      !(STRef s [(Location SourcePos, [Pat (Pinned VCObjectHash) SourcePos])])
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
            cap1 = max (cap0 * 2) $ i + 1
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
    UFLink parent ->
      ufFind parent >>= \root -> do
        -- Path compression
        when (root /= parent) . withStore $
          \v -> Vector.Mutable.write v i $ UFLink root
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
      unify mempty old t

-------------------------------------------------------------------------------
-- Zonking
-------------------------------------------------------------------------------

-- | Walk a type, replacing 'TVar's via 'ufProbe' and recursing structurally.
-- For 'TRecord', when the row variable resolves to another record, merge
-- the fields and continue zonking the new row variable (mirrors
-- @substMap s (TRecord ...)@ in @Syntax.hs@).
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

-- | Check whether @tv@ occurs in @t@. Callers must zonk @t@ first;
-- 'unify' already does this before reaching the occurs-check branches.
occursIn :: TV -> InfernoType -> Bool
occursIn tv = Set.member tv . ftv

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
        when (a `occursIn` t) $ throwError infiniteErrs
        ufBind a t
      t (TVar a) -> do
        when (a `occursIn` t) $ throwError infiniteErrs
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

-- | One side of a record-field merge: remaining sorted fields,
-- the row variable tail, and accumulated new fields.
data RecordSide = RecordSide
  { fields :: ![(Ident, InfernoType)]
  , ror :: !RestOfRecord
  , new :: ![(Ident, InfernoType)]
  }

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
-- Initialization
-------------------------------------------------------------------------------

-- | Allocate all mutable refs for a fresh inference run.
initRefs ::
  forall s m.
  Map ModuleName (PinnedModule m) ->
  Set TypeClass ->
  ST s (InferRefs s)
initRefs mods tyClasses = do
  count <- newSTRef 0
  v <- Vector.Mutable.replicate 128 $ UFRoot 0 Nothing
  ufStore <- newSTRef v
  typeMap <- newSTRef mempty
  modules <- newSTRef $ mods <&> \m -> m{moduleObjects = ()}
  patternsToCheck <- newSTRef mempty
  deferred <- newSTRef mempty
  pure
    InferRefs
      { count
      , ufStore
      , typeMap
      , modules
      , tyClasses
      , patternsToCheck
      , deferred
      }
