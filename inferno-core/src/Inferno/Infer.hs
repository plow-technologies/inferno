{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
  )
where

import Control.Monad (when)
import Control.Monad.Except
  ( Except,
    ExceptT,
    MonadError (catchError, throwError),
    foldM,
    forM,
    forM_,
    runExcept,
    runExceptT,
  )
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader
  ( MonadReader (ask, local),
    ReaderT (ReaderT, runReaderT),
  )
import Control.Monad.State
  ( MonadState (get, put, state),
    StateT (StateT, runStateT),
    evalStateT,
    execState,
    modify,
  )
import Data.Bifunctor (bimap)
import qualified Data.Bimap as Bimap
import Data.Either (partitionEithers, rights)
import Data.Generics.Product (HasType, getTyped, setTyped)
import Data.List (find, unzip4) -- intercalate
import qualified Data.List.NonEmpty as NEList
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Debug.Trace (trace)
import Inferno.Infer.Env (Env (..), TypeMetadata (..), closeOver, closeOverType)
import qualified Inferno.Infer.Env as Env
import Inferno.Infer.Error (TypeError (..), getLocFromErrs, getTypeClassFromErrs)
import Inferno.Infer.Exhaustiveness
  ( Pattern (..),
    cEmpty,
    cEnum,
    cInf,
    cOne,
    cTuple,
    checkUsefullness,
    exhaustive,
    mkEnumArrayPat,
    mkEnumText,
  )
import Inferno.Module.Builtin (builtinModule, emptyHash, oneHash)
import Inferno.Types.Module (Module (..), PinnedModule, pinnedModuleHashToTy)
import Inferno.Types.Syntax
  ( BlockUtils (blockPosition, removeComments),
    ElementPosition (elementPosition),
    Expr (..),
    ExtIdent (..),
    Ident (..),
    ImplExpl (Expl, Impl),
    Lit (LDouble, LHex, LInt, LText),
    ModuleName (..),
    Pat (..),
    Scoped (..),
    fromEitherList,
    fromScoped,
    incSourceCol,
    patternToExpr,
    substInternalIdents,
    tListFromList,
    tListToList,
    toEitherList,
  )
import Inferno.Types.Type
  ( BaseType (TEnum),
    ImplType (..),
    InfernoType (..),
    Subst (..),
    Substitutable (..),
    TCScheme (..),
    TV,
    TypeClass (TypeClass),
    typeBool,
    typeDouble,
    typeInt,
    typeText,
    typeWord64,
    var,
    (.->),
  )
import Inferno.Types.VersionControl (Pinned (..), VCObjectHash, pinnedToMaybe, vcHash)
-- import Inferno.Utils.Prettyprinter (renderPretty)
-- import Prettyprinter (pretty)
import qualified Picosat
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (SourcePos (..), initialPos)

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Inference monad
type Infer a =
  ( ReaderT
      Env -- Typing environment
      ( StateT -- Inference state
          InferState
          ( Except -- Inference errors
              [TypeError SourcePos]
          )
      )
      a -- Result
  )

-- | Inference state
data InferState = InferState
  { count :: Int,
    typeMap :: Map.Map (Location SourcePos) (TypeMetadata (Set.Set TypeClass, ImplType)),
    modules :: Map.Map ModuleName (Module ()),
    typeClasses :: Set.Set TypeClass,
    patternsToCheck :: [(Location SourcePos, [Pat (Pinned VCObjectHash) SourcePos])]
  }

-- | Initial inference state
initInfer :: InferState
initInfer = InferState {count = 0, typeMap = Map.empty, modules = Map.empty, typeClasses = Set.empty, patternsToCheck = []}

type Constraint = Either (InfernoType, InfernoType, [TypeError SourcePos]) (Location SourcePos, TypeClass)

type Unifier = (Subst, [Constraint])

-- | Constraint solver monad
type Solve a = ReaderT (Set.Set TypeClass) (ExceptT [TypeError SourcePos] Identity) a

type SolveState st a = ReaderT (Set.Set TypeClass) (StateT st (ExceptT [TypeError SourcePos] Identity)) a

type Location a = (a, a)

instance Substitutable Constraint where
  apply s (Left (t1, t2, es)) = Left (apply s t1, apply s t2, es)
  apply s (Right (loc, tc)) = Right (loc, apply s tc)
  ftv (Left (t1, t2, _)) = ftv t1 `Set.union` ftv t2
  ftv (Right (_, tc)) = ftv tc

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

filterInstantiatedTypeClasses :: Set.Set TypeClass -> Set.Set TypeClass
filterInstantiatedTypeClasses = Set.filter $ not . Set.null . ftv

mkPattern :: Pat (Pinned VCObjectHash) SourcePos -> Pattern
mkPattern = \case
  PVar _ _ -> W
  PEnum _ Local _ns _ident -> error $ "internal error. cannot convert unpinned enum into a pattern"
  PEnum _ hash _ns ident@(Ident i) -> cEnum (vcHash (ident, fromJust $ pinnedToMaybe hash)) i
  PLit _ l -> case l of
    LInt v -> cInf v
    LDouble v -> cInf v
    LHex v -> cInf v
    LText v -> cInf $ mkEnumText v
  POne _ p -> cOne $ mkPattern p
  PEmpty _ -> cEmpty
  PArray _ ps _ -> cInf $ mkEnumArrayPat ps
  PTuple _ ps _ -> cTuple $ map (mkPattern . fst) $ tListToList ps
  PCommentAbove _ p -> mkPattern p
  PCommentAfter p _ -> mkPattern p
  PCommentBelow p _ -> mkPattern p

checkExhaustivenessAndUsefullness :: Map.Map VCObjectHash (Set.Set (VCObjectHash, Text.Text)) -> Location SourcePos -> [Pat (Pinned VCObjectHash) SourcePos] -> [TypeError SourcePos]
checkExhaustivenessAndUsefullness enum_sigs loc patts =
  let patternsOfPatts = map ((: []) . mkPattern) patts
   in case exhaustive enum_sigs patternsOfPatts of
        Just ps -> [NonExhaustivePatternMatch p loc | p <- ps]
        Nothing ->
          let uErrs = checkUsefullness enum_sigs patternsOfPatts
           in map
                ( \(i, j) ->
                    let loc' = blockPosition $ patts !! i
                     in let pat = patts !! j
                         in if i == j then UselessPattern Nothing loc' else UselessPattern (Just pat) loc'
                )
                uErrs

-- | Run the inference monad
runInfer :: Env -> Set.Set TypeClass -> Map.Map ModuleName (Module m) -> Infer r -> Either [TypeError SourcePos] (r, InferState)
runInfer env allClasses allModules m =
  runExcept $
    runStateT
      (runReaderT m env)
      initInfer
        { modules = Map.map (\m' -> m' {moduleObjects = ()}) allModules,
          typeClasses = allClasses
        }

-- Given a map of implicit types containing `rep of <ty>` variables, and an expression `e`
-- we want to either substitute any implicit variable `?var$n : rep of <ty>` with a `RuntimeRep <ty>`,
-- provided that `<ty>` contains no free variables
-- otherwise we want to create a closure `fun var$n -> e[var$n/?var$n]`
closeOverTypeReps :: Map.Map ExtIdent InfernoType -> Expr (Pinned VCObjectHash) SourcePos -> (Maybe TypeClass, Map.Map ExtIdent InfernoType, Expr (Pinned VCObjectHash) SourcePos)
closeOverTypeReps implTys expr =
  let (tReps, rest) = flip Map.partition implTys $ \case
        TRep _ -> True
        _ -> False
   in if Map.null tReps
        then (Nothing, implTys, expr)
        else
          let -- we partition the types to those that have been fully inferred and ones where we are lacking the type-rep information
              (fullyInstantiated, withTypeHole) = Map.partition (Set.null . ftv) tReps
              -- next, we create a Map Int (Either Int InfernoType) where all the fully instantiated `TRep ty`s get mapped to a `Right ty`
              fullyInstantiatedMap =
                Map.foldrWithKey
                  ( \k v m -> case (k, v) of
                      (ExtIdent (Left i), TRep ty) -> Map.insert i (Right ty) m
                      _ -> m
                  )
                  mempty
                  fullyInstantiated

              -- we group all the types with holes
              withTypeHoleGrouped = NEList.groupBy (\(_, ty1) (_, ty2) -> ty1 == ty2) $ Map.toList withTypeHole
              -- each [(var$i_1, _),...,(var$i_n, _)] gets turned into the map {i_1 -> Left i_1, ... i_n -> Left i_1}
              substMap =
                foldr
                  ( \vs m -> case NEList.head vs of
                      (ExtIdent (Left i), _) ->
                        foldr
                          ( \(v, _) m' -> case v of
                              ExtIdent (Left j) -> Map.insert j (Left i) m'
                              _ -> m'
                          )
                          m
                          vs
                      _ -> m
                  )
                  fullyInstantiatedMap
                  withTypeHoleGrouped

              (pos, _) = blockPosition expr
              expr' = substInternalIdents substMap expr
           in case Map.toList withTypeHole of
                [] -> (Nothing, rest, expr')
                -- if we have any type holes, we need to wrap the expression in a lambda and add a `requires rep on ...` typeclass
                -- capturing all the runtime reps that the expression needs
                _ : _ ->
                  let lamList = fmap (\merged -> let (v, _) = NEList.head merged in (pos, Just v)) $ NEList.fromList $ withTypeHoleGrouped
                   in ( Just $
                          TypeClass "rep" $
                            map
                              ( \((_, ty) NEList.:| _) -> case ty of
                                  TRep ty' -> ty'
                                  _ -> ty
                              )
                              withTypeHoleGrouped,
                        rest,
                        Lam pos lamList pos expr'
                      )

-- | Solve for the top level type of an expression in a given environment
inferExpr ::
  Map.Map ModuleName (PinnedModule m) ->
  Expr (Pinned VCObjectHash) SourcePos ->
  Either
    [TypeError SourcePos]
    ( Expr (Pinned VCObjectHash) SourcePos,
      TCScheme,
      Map.Map (Location SourcePos) (TypeMetadata TCScheme)
    )
inferExpr allModules expr =
  let (env, inScopeClasses) = openBuiltinModuleAndAddPinnedTypes allModules
   in case runInfer env inScopeClasses allModules (infer expr) of
        -- if we threw errors whilst inferring, rethrow
        Left err -> Left err
        Right ((expr', ty, cs), InferState {..}) ->
          -- trace ("new expr: " <> (Text.unpack . renderPretty) expr') $
          --   trace
          --     ( "ty: " <> (Text.unpack . renderPretty) ty
          --         <> "\ncs: "
          --         <> (intercalate "\n" $ map show $ Set.toList cs)
          --     )
          --     $
          -- case runSolve typeClasses $ filter (\case { Right (_, TypeClass "rep" _) -> False; _ -> True }) $ Set.toList cs of
          case runSolve typeClasses $ Set.toList cs of
            Left errs -> Left errs
            Right subst ->
              -- trace ("substs: " <> show subst) $
              -- trace ("patternsToCheck: " <> show patternsToCheck) $
              case concatMap (uncurry $ checkExhaustivenessAndUsefullness enumSigs) patternsToCheck of
                errs@(_ : _) -> Left errs
                _ -> do
                  -- get type classes and type from solved constraints
                  let cls = filterInstantiatedTypeClasses $ Set.map (apply subst . snd) $ Set.fromList $ rights $ Set.toList cs
                  let substitutedTy@(ImplType implTys tyBody) = apply subst ty
                  -- get current type variables
                  let tvs = ftv substitutedTy `Set.union` (Set.unions $ Set.elems $ Set.map ftv cls)
                  let res substNew (mRepTyCls, implTys', expr'') =
                        let finalTy =
                              closeOver
                                ((filterInstantiatedTypeClasses $ Set.map (apply substNew) cls) `Set.union` (maybe mempty Set.singleton mRepTyCls))
                                $ apply substNew
                                $ ImplType implTys' tyBody
                         in Right $
                              ( expr'',
                                finalTy,
                                Map.map
                                  (\meta@TypeMetadata {ty = (tcs, t)} -> meta {ty = closeOver (filterInstantiatedTypeClasses $ Set.map (apply $ substNew <> subst) tcs) $ apply (substNew <> subst) t})
                                  typeMap
                              )

                  if -- trace ("type classes: " <> show cls) $
                  Set.null cls
                    then res mempty $ closeOverTypeReps implTys expr'
                    else case findTypeClassWitnesses typeClasses (Just 2) cls tvs of
                      [] -> Left [CouldNotFindTypeclassWitness cls $ blockPosition expr]
                      -- we attempt to find two type assignments. If there is only one satisfying assignment for all the type-classes, we automatically substitute the instantiations
                      [subst'] -> res subst' $ closeOverTypeReps (Map.map (apply subst') implTys) expr'
                      -- even if there isn't a unique solution, we can still safely apply the substitutions to any types which do not transitively depend on the input or output type variables
                      -- e.g. for `let x = 3.2 in x + 2` it does not matter whether the type of `2` is an int or a double, because the final type of the whole expression won't change
                      (Subst s) : _ ->
                        let ftvsDependentOnOuterType =
                              Set.foldl
                                ( \ftvsTransClosure c ->
                                    let ftvCls = ftv c
                                     in if Set.null $ ftvCls `Set.intersection` ftvsTransClosure
                                          then ftvsTransClosure
                                          else ftvCls `Set.union` ftvsTransClosure
                                )
                                -- start with the body of the type, i.e. in `forall a_1 ... a_n {requires ..., implicit ...} => t` get the type variables in `t`
                                -- as well as any implicit arguments which aren't internal, since those are used for tracking type-reps
                                (ftv tyBody `Set.union` (Map.foldrWithKey (\(ExtIdent ident) t ftvs -> case ident of Left _ -> ftvs; Right _ -> ftv t `Set.union` ftvs) mempty implTys))
                                cls
                            subst' = Subst $ Set.foldr Map.delete s ftvsDependentOnOuterType
                         in -- trace ("type ftvsDependentOnOuterType: " <> show ftvsDependentOnOuterType) $
                            res subst' $ closeOverTypeReps (Map.map (apply subst') implTys) expr'
  where
    enumSigs :: Map.Map VCObjectHash (Set.Set (VCObjectHash, Text.Text))
    enumSigs =
      Map.foldrWithKey
        ( \h (ForallTC _ _ (ImplType _ ty)) m -> case ty of
            TBase (TEnum _n cs) ->
              let allConstrHashes = Set.map (\c@(Ident i) -> (vcHash (c, h), i)) cs
               in Map.fromList [(cH, allConstrHashes) | (cH, _) <- Set.toList allConstrHashes] `Map.union` m
            _ -> m
        )
        mempty
        $ Map.map ty
        $ Map.unions
        $ pinnedModuleHashToTy builtinModule : (map pinnedModuleHashToTy $ Map.elems allModules)
    openBuiltinModuleAndAddPinnedTypes :: Map.Map ModuleName (PinnedModule m) -> (Env, Set.Set TypeClass)
    openBuiltinModuleAndAddPinnedTypes modules =
      let Module {moduleTypeClasses = tyCls, moduleObjects = (_, tys, _)} = builtinModule
       in ( Env.empty
              { Env.pinnedTypes = Map.unions $ tys : [pinnedModuleHashToTy m | m <- Map.elems modules]
              },
            tyCls `Set.union` Set.unions [tc | Module {moduleTypeClasses = tc} <- Map.elems modules]
          )

-- | Given a type signature and some concrete assignment of types (assumes inputTys and outputTy have no free variables)
-- | this function computes the runtime reps
inferTypeReps :: Set.Set TypeClass -> TCScheme -> [InfernoType] -> InfernoType -> Either [TypeError SourcePos] [InfernoType]
inferTypeReps allTypeClasses (ForallTC tvs tyCls (ImplType _impl ty)) inputTys outputTy =
  let cs =
        [Right (dummyPos, c) | c@(TypeClass nm _) <- Set.toList tyCls, nm /= "rep"]
          ++ mkConstraints ty inputTys
   in case runSolve allTypeClasses cs of
        Left errs -> Left errs
        Right subst ->
          let tyClsSubst = Set.map (apply subst) tyCls
           in case find (\(TypeClass nm _) -> nm == "rep") $ Set.toList tyClsSubst of
                Nothing -> pure []
                Just rep@(TypeClass _ runtimeRepTys) ->
                  if Set.null $ ftv rep
                    then pure runtimeRepTys
                    else case findTypeClassWitnesses
                      allTypeClasses
                      (Just 1)
                      (Set.filter (\case TypeClass "rep" _ -> False; _ -> True) tyClsSubst)
                      (Set.fromList tvs) of
                      [] -> Left [CouldNotFindTypeclassWitness tyClsSubst dummyPos]
                      subst' : _ -> pure $ apply subst' runtimeRepTys
  where
    mkConstraints (TArr t1 t2) (x : xs) = Left (t1, x, []) : mkConstraints t2 xs
    mkConstraints t [] = [Left (t, outputTy, [])]
    mkConstraints _ _ = error "mkConstraints: invalid input params length"

    dummyPos = let pos = initialPos "" in (pos, pos)

inferPossibleTypes :: Set.Set TypeClass -> TCScheme -> [Maybe InfernoType] -> Maybe InfernoType -> Either [TypeError SourcePos] ([[InfernoType]], [InfernoType])
inferPossibleTypes allTypeClasses (ForallTC _ tyCls (ImplType _impl ty)) inputTys outputTy =
  let cs =
        [Right (dummyPos, c) | c@(TypeClass nm _) <- Set.toList tyCls, nm /= "rep"]
          ++ mkMaybeConstraints ty inputTys
   in case runSolve allTypeClasses cs of
        Left errs -> Left errs
        Right subst -> do
          let tyClsSubst = Set.map (apply subst) $ Set.filter (\case TypeClass "rep" _ -> False; _ -> True) tyCls
          let (inTysFromSig, outTyFromSig) = gatherArgs $ apply subst ty
          let findAllPossibleTypes (supplied, t) = case supplied of
                Just t' -> pure [t']
                Nothing ->
                  let tvs = ftv t
                   in if Set.null tvs
                        then pure [t]
                        else case findTypeClassWitnesses allTypeClasses (Just 100) tyClsSubst tvs of
                          [] -> Left [CouldNotFindTypeclassWitness tyClsSubst dummyPos]
                          substs -> pure [apply sub t | sub <- substs]

          possibleInTysFromSig <- forM (zip inputTys inTysFromSig) findAllPossibleTypes
          (possibleInTysFromSig,) <$> findAllPossibleTypes (outputTy, outTyFromSig)
  where
    gatherArgs (TArr t1 t2) = bimap (t1 :) id $ gatherArgs t2
    gatherArgs x = ([], x)
    mkMaybeConstraints (TArr t1 t2) (Just x : xs) = Left (t1, x, []) : mkMaybeConstraints t2 xs
    mkMaybeConstraints (TArr _ t2) (Nothing : xs) = mkMaybeConstraints t2 xs
    mkMaybeConstraints t [] = case outputTy of
      Just t' -> [Left (t, t', [])]
      Nothing -> []
    mkMaybeConstraints _ _ = error "mkConstraints: invalid input params length"

    dummyPos = let pos = initialPos "" in (pos, pos)

-- | Extend type environment
inEnv :: (ExtIdent, TypeMetadata TCScheme) -> Infer a -> Infer a
inEnv (x, meta) m = do
  let scope e = (Env.remove e x) `Env.extend` (x, meta)
  local scope m

-- | Lookup type in the environment
lookupEnv :: Location SourcePos -> Either VCObjectHash ExtIdent -> Infer (TypeMetadata (Set.Set TypeClass, ImplType))
lookupEnv loc x = do
  env <- ask
  case either (flip Env.lookupPinned env) (flip Env.lookup env) x of
    Nothing ->
      throwError
        [ either
            (\hsh -> UnboundNameInNamespace LocalScope (Left hsh) loc)
            (\i -> UnboundExtIdent LocalScope i loc)
            x
        ]
    Just meta -> do
      iTy <- instantiate $ ty meta
      return meta {ty = iTy}

mergeImplicitMaps :: Location SourcePos -> [Map.Map ExtIdent InfernoType] -> (Map.Map ExtIdent InfernoType, [Constraint])
mergeImplicitMaps loc =
  foldr
    ( \m (mAll, cs) ->
        let cs' = Map.elems $ Map.intersectionWithKey (\ident t1 t2 -> tyConstr t1 t2 [ImplicitVarTypeOverlap mempty ident t1 t2 loc]) mAll m
         in (mAll `Map.union` m, cs' ++ cs)
    )
    (Map.empty, [])

fresh :: Infer InfernoType
fresh = do
  s@InferState {..} <- get
  put s {count = count + 1}
  return $ var count

freshRaw :: Infer Int
freshRaw = do
  s@InferState {..} <- get
  put s {count = count + 1}
  return count

attachTypeToPosition :: Location SourcePos -> TypeMetadata (Set.Set TypeClass, ImplType) -> Infer ()
attachTypeToPosition k meta =
  modify (\s -> s {typeMap = Map.insert k meta $ typeMap s})

addCasePatterns :: Location SourcePos -> [Pat (Pinned VCObjectHash) SourcePos] -> Infer ()
addCasePatterns k pttrns =
  modify
    ( \s ->
        s
          { patternsToCheck = (k, pttrns) : patternsToCheck s
          }
    )

instantiate :: TCScheme -> Infer (Set.Set TypeClass, ImplType)
instantiate (ForallTC as tcs t) = do
  as' <- mapM (const fresh) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ (Set.map (apply s) tcs, apply s t)

opGetTyComponents :: ImplType -> (InfernoType, InfernoType, InfernoType)
opGetTyComponents (ImplType _ (t1 `TArr` (t2 `TArr` t3))) = (t1, t2, t3)
opGetTyComponents _ = error "Invalid op type signature"

preOpGetTyComponents :: ImplType -> (InfernoType, InfernoType)
preOpGetTyComponents (ImplType _ (t1 `TArr` t2)) = (t1, t2)
preOpGetTyComponents _ = error "Invalid pre-op type signature"

tyConstr :: a -> b -> c -> Either (a, b, c) d
tyConstr t1 t2 es = Left (t1, t2, es)

inferLit :: Expr (Pinned VCObjectHash) SourcePos -> Location SourcePos -> Lit -> InfernoType -> Infer (Expr (Pinned VCObjectHash) SourcePos, ImplType, Set.Set Constraint)
inferLit expr loc l t = do
  attachTypeToPosition loc $
    TypeMetadata
      { identExpr = Lit () l,
        ty = (Set.empty, ImplType Map.empty t),
        docs = Nothing
      }
  return (expr, ImplType Map.empty t, Set.empty)

infer :: Expr (Pinned VCObjectHash) SourcePos -> Infer (Expr (Pinned VCObjectHash) SourcePos, ImplType, Set.Set Constraint)
infer expr =
  let exprLoc = blockPosition expr
   in case expr of
        Lit pos l@(LInt _) -> do
          tv <- fresh
          let tyCls = TypeClass "numeric" [tv]
          attachTypeToPosition exprLoc $
            TypeMetadata
              { identExpr = Lit () l,
                ty = (Set.singleton tyCls, ImplType Map.empty tv),
                docs = Nothing
              }

          i <- ExtIdent . Left <$> freshRaw
          return (App expr (Var pos Local LocalScope $ Impl i), ImplType (Map.fromList [(i, TRep tv)]) tv, Set.singleton $ Right (exprLoc, tyCls))
        Lit _ l ->
          inferLit
            expr
            exprLoc
            l
            (handleLit l)
          where
            handleLit (LDouble _) = typeDouble
            handleLit (LHex _) = typeWord64
            handleLit (LText _) = typeText
            handleLit (LInt _) = undefined
        Var pos mHash _modNm (Expl x) -> do
          meta <- lookupEnv exprLoc (maybe (Right x) Left $ pinnedToMaybe mHash)
          let (tcs, t@(ImplType impl t'')) = ty meta
          attachTypeToPosition exprLoc meta
          (expr', t') <- case find (\(TypeClass nm _) -> nm == "rep") $ Set.toList tcs of
            Just (TypeClass _ runtimeRepTys) -> do
              implRepTyps <- forM runtimeRepTys $ \repTy -> do
                i <- freshRaw
                pure (ExtIdent $ Left i, TRep repTy)
              let (vars, _) = unzip implRepTyps

              pure (foldl App expr $ map (Var pos Local LocalScope . Impl) vars, ImplType (impl `Map.union` Map.fromList implRepTyps) t'')
            Nothing -> pure (expr, t)
          return (expr', t', Set.map (Right . (exprLoc,)) $ Set.filter (\case TypeClass "rep" _ -> False; _ -> True) tcs)
        Var _ _ _ (Impl x) -> do
          tv <- fresh
          attachTypeToPosition
            exprLoc
            TypeMetadata
              { identExpr = bimap (const ()) (const ()) expr,
                ty = (Set.empty, ImplType (Map.fromList [(x, tv)]) tv),
                docs = Nothing
              }
          return (expr, ImplType (Map.fromList [(x, tv)]) tv, Set.empty)
        OpVar _ mHash _ _ -> do
          meta <- lookupEnv exprLoc (maybe (error "internal error, op vars must always be pinned!!") Left $ pinnedToMaybe mHash)
          let (tcs, t) = ty meta
          attachTypeToPosition exprLoc meta
          return (expr, t, Set.map (Right . (exprLoc,)) tcs)
        TypeRep _pos t -> return (expr, ImplType mempty $ TRep t, Set.empty)
        Enum _ mHash _ _ -> do
          meta <- lookupEnv exprLoc (maybe (error "internal error, enums must always be pinned!!") Left $ pinnedToMaybe mHash)
          let (_, t) = ty meta
          attachTypeToPosition exprLoc meta {identExpr = bimap (const ()) (const ()) $ expr}
          return (expr, t, Set.empty)
        InterpolatedString p1 xs p2 -> do
          attachTypeToPosition
            exprLoc
            TypeMetadata
              { identExpr = bimap (const ()) (const ()) $ removeComments expr,
                ty = (Set.empty, ImplType Map.empty typeText),
                docs = Nothing
              }
          (xs', is, css) <-
            unzip3
              <$> ( forM (toEitherList xs) $ \case
                      Left str -> return (Left str, Map.empty, Set.empty)
                      Right (p3, e, p4) -> (\(e', ImplType is _t, cs) -> (Right (p3, e', p4), is, cs)) <$> infer e
                  )
          let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) is
          return (InterpolatedString p1 (fromEitherList xs') p2, ImplType isMerged typeText, Set.unions css `Set.union` Set.fromList ics)
        Record p1 fes p2 -> do
          let (fs, es) = unzip $ map (\(f, e, p) -> (f, (e, p))) fes
          (es', impls, tys, cs) <- go es
          let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) impls
          let inferredTy = ImplType isMerged $ TRecord $ Map.fromList $ zip fs tys
          let fes' = zipWith (\f (e, p) -> (f, e, p)) fs es'

          attachTypeToPosition
            exprLoc
            TypeMetadata
              { identExpr = bimap (const ()) (const ()) $ removeComments expr,
                ty = (Set.empty, inferredTy),
                docs = Nothing
              }
          return
            ( Record p1 fes' p2,
              inferredTy,
              Set.fromList ics `Set.union` cs
            )
          where
            go [] = return ([], [], [], Set.empty)
            go ((e', p3) : es') = do
              (e'', ImplType i t, cs) <- infer e'
              (es'', impls, tRest, csRest) <- go es'
              return ((e'', p3) : es'', i : impls, t : tRest, cs `Set.union` csRest)
        RecordField p_r (Ident r) p_f (Ident f) -> do
          (_e', ImplType i_r t_r, cs_r) <- infer $ Var p_r Local LocalScope $ Expl $ ExtIdent $ Right r
          case t_r of
            TRecord tys ->
              case Map.lookup (Ident f) tys of
                Just t ->
                  return (expr, ImplType i_r t, cs_r)
                Nothing -> error $ "Record does not have field " <> Text.unpack f -- TODO proper Error message here?
            _ ->
              -- TODO if we want to support this, we need a way of adding a IsRecordWithField constraint to solver here
              error "Record field access on non-record typed variable"
        Array _ [] _ -> do
          tv <- fresh
          let meta =
                TypeMetadata
                  { identExpr = bimap (const ()) (const ()) $ removeComments expr,
                    ty = (Set.empty, ImplType Map.empty $ TArray tv),
                    docs = Nothing
                  }
          let (_, t) = ty meta
          attachTypeToPosition exprLoc meta
          return (expr, t, Set.empty)
        Array p1 ((e, p2) : es) p3 -> do
          (e', ImplType i t, cs) <- infer e
          (es', impls, cs') <- go t es
          let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) $ i : impls
          let inferredTy = ImplType isMerged $ TArray t
          attachTypeToPosition
            exprLoc
            TypeMetadata
              { identExpr = bimap (const ()) (const ()) $ removeComments expr,
                ty = (Set.empty, inferredTy),
                docs = Nothing
              }

          return
            ( Array p1 ((e', p2) : es') p3,
              inferredTy,
              Set.fromList ics `Set.union` cs `Set.union` cs'
            )
          where
            go _t [] = return ([], [], Set.empty)
            go t ((e', p4) : es') = do
              (e'', ImplType i t', cs) <- infer e'
              (es'', impls, csRest) <- go t es'
              return
                ( (e'', p4) : es'',
                  i : impls,
                  cs
                    `Set.union` csRest
                    `Set.union` Set.fromList
                      [ tyConstr
                          t
                          t'
                          [ UnificationFail
                              (Set.fromList . map snd . rights . Set.toList $ cs `Set.union` csRest)
                              t
                              t'
                              $ blockPosition e'
                          ]
                      ]
                )
        ArrayComp p1 e p2 sels cond p3 -> do
          _ <- checkVariableOverlap $ NEList.toList sels
          (sels', vars, is, css) <- unzip4 <$> go (NEList.toList sels) id

          (e', ImplType i_e t_e, c_e) <- foldr inEnv (infer e) vars

          (cond', i_cond, c_cond) <- case cond of
            Just (p4, e_cond) -> do
              (e_cond', ImplType i_cond t_cond, c_cond) <- foldr inEnv (infer e_cond) vars
              return
                ( Just (p4, e_cond'),
                  i_cond,
                  c_cond
                    `Set.union` Set.singleton (tyConstr t_cond typeBool [UnificationFail (Set.fromList . map snd . rights . Set.toList $ c_cond) t_cond typeBool $ blockPosition e_cond])
                )
            Nothing -> return (Nothing, Map.empty, Set.empty)

          let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) $ [i_e, i_cond] ++ is
          return
            ( ArrayComp p1 e' p2 (NEList.fromList sels') cond' p3,
              ImplType isMerged (TArray t_e),
              Set.fromList ics
                `Set.union` c_e
                `Set.union` c_cond
                `Set.union` Set.unions css
            )
          where
            go [] _ = return []
            go ((pos, Ident x, p4, e_s, p5) : xs) f = do
              (e_s', ImplType i_s t_s, c_s) <- f $ infer e_s
              tv <- fresh
              attachTypeToPosition
                (elementPosition pos $ Ident x)
                TypeMetadata
                  { identExpr = Var () () LocalScope $ Expl $ ExtIdent $ Right x,
                    ty = (Set.fromList $ map snd $ rights $ Set.toList c_s, ImplType i_s tv),
                    docs = Nothing
                  }
              let newEnv =
                    ( ExtIdent $ Right x,
                      TypeMetadata
                        { identExpr = Var () () LocalScope $ Expl $ ExtIdent $ Right x,
                          ty = ForallTC [] (Set.fromList $ map snd $ rights $ Set.toList c_s) $ ImplType i_s tv,
                          docs = Nothing
                        }
                    )
              rest <- go xs (inEnv newEnv . f)
              return $
                ( (pos, Ident x, p4, e_s', p5),
                  newEnv,
                  i_s,
                  c_s `Set.union` Set.singleton (tyConstr t_s (TArray tv) [UnificationFail (Set.fromList . map snd . rights . Set.toList $ c_s) t_s (TArray tv) $ blockPosition e_s])
                )
                  : rest

            checkVariableOverlap :: [(SourcePos, Ident, SourcePos, b, Maybe SourcePos)] -> Infer ()
            checkVariableOverlap = \case
              [] -> return ()
              (loc, x, _, _e, _) : xs -> case find (\(_, x', _, _, _) -> x == x') xs of
                Just (loc', x', _, _, _) -> throwError [VarMultipleOccurrence x (elementPosition loc x) (elementPosition loc' x')]
                Nothing -> checkVariableOverlap xs
        Lam p1 args p2 e -> do
          (e', ty, cs) <- go $ NEList.toList args
          return (Lam p1 args p2 e', ty, cs)
          where
            go = \case
              [] -> infer e
              (pos, Just x) : xs -> do
                tv <- fresh
                let newEnv =
                      ( x,
                        TypeMetadata
                          { identExpr = Var () () LocalScope $ Expl x,
                            ty = ForallTC [] Set.empty $ ImplType Map.empty tv,
                            docs = Nothing
                          }
                      )
                (e', ImplType is t, cs) <- inEnv newEnv $ go xs
                case x of
                  ExtIdent (Left _) -> pure ()
                  ExtIdent (Right i) ->
                    attachTypeToPosition
                      (elementPosition pos $ Just $ Ident i)
                      TypeMetadata
                        { identExpr = Var () () LocalScope $ Expl x,
                          ty = (Set.empty, ImplType Map.empty tv),
                          docs = Nothing
                        }
                return (e', ImplType is $ tv `TArr` t, cs)
              (pos, Nothing) : xs -> do
                tv <- fresh
                attachTypeToPosition
                  (elementPosition pos (Nothing :: Maybe Ident))
                  TypeMetadata
                    { identExpr = Var () () LocalScope $ Expl $ ExtIdent $ Right "_",
                      ty = (Set.empty, ImplType Map.empty tv),
                      docs = Nothing
                    }
                (e', ImplType is t, cs) <- go xs
                return (e', ImplType is $ tv `TArr` t, cs)
        App e1 e2 -> do
          (e1', ImplType i1 t1, c1) <- infer e1
          (e2', ImplType i2 t2, c2) <- infer e2

          case t1 of
            t1a `TArr` t1b -> do
              tv <- fresh
              let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) [i1, i2]
                  tyCls = Set.fromList $ map snd $ rights $ Set.toList $ c1 `Set.union` c2
              return
                ( App e1' e2',
                  ImplType isMerged tv,
                  Set.fromList ics
                    `Set.union` c1
                    `Set.union` c2
                    `Set.union` Set.fromList
                      [ tyConstr t1a t2 [UnificationFail tyCls t1a t2 $ blockPosition e2],
                        tyConstr t1b tv [UnificationFail tyCls t1b tv $ blockPosition expr]
                      ]
                )
            _ -> do
              tv <- fresh
              let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) [i1, i2]
                  tyCls = Set.fromList $ map snd $ rights $ Set.toList $ c1 `Set.union` c2
              -- if we end up on this branch, we will be throwing a unification error and
              -- want to highlight e1, thus we attach `blockPosition e1` to the error
              return
                ( App e1' e2',
                  ImplType isMerged tv,
                  Set.fromList ics
                    `Set.union` c1
                    `Set.union` c2
                    `Set.union` Set.fromList
                      [ tyConstr t1 (t2 `TArr` tv) [ExpectedFunction tyCls (t2 `TArr` tv) t1 $ blockPosition e1]
                      ]
                )
        LetAnnot p1 loc x pT t p2 e1 p3 e2 -> do
          (e1', ImplType i1 t1, c1) <- infer e1
          (tcs, (ImplType iT tT)) <- instantiate t
          let tyCls = Set.fromList $ map snd $ rights $ Set.toList c1
          attachTypeToPosition
            (elementPosition loc $ Expl x)
            TypeMetadata
              { identExpr = Var () () LocalScope $ Expl x,
                -- ty = (tyCls, ImplType i1 t1),
                ty = (tcs, (ImplType iT tT)),
                docs = Nothing
              }
          let newEnv =
                ( x,
                  TypeMetadata
                    { identExpr = Var () () LocalScope $ Expl x,
                      -- ty = ForallTC [] tyCls $ ImplType i1 t1,
                      ty = ForallTC [] tcs (ImplType iT tT),
                      docs = Nothing
                    }
                )
          (e2', ImplType i2 t2, c2) <- inEnv newEnv $ infer e2
          let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) [i1, i2, iT]
          return
            ( LetAnnot p1 loc x pT t p2 e1' p3 e2',
              ImplType isMerged t2,
              Set.fromList ics
                `Set.union` c1
                `Set.union` c2
                -- Type of e1 == type annotation
                `Set.union` Set.fromList [tyConstr t1 tT [AnnotationUnificationFail tyCls t1 tT $ blockPosition e1]]
                -- Type class constraints from type annotation TODO filter out reps?
                `Set.union` Set.map (Right . (exprLoc,)) tcs
            )
        -- non generalized let
        Let p1 loc (Expl x) p2 e1 p3 e2 -> do
          (e1', ImplType i1 t1, c1) <- infer e1
          let tyCls = Set.fromList $ map snd $ rights $ Set.toList c1
          attachTypeToPosition
            (elementPosition loc $ Expl x)
            TypeMetadata
              { identExpr = Var () () LocalScope $ Expl x,
                ty = (tyCls, ImplType i1 t1),
                docs = Nothing
              }

          let newEnv =
                ( x,
                  TypeMetadata
                    { identExpr = Var () () LocalScope $ Expl x,
                      ty = ForallTC [] tyCls $ ImplType i1 t1,
                      docs = Nothing
                    }
                )
          (e2', ImplType i2 t2, c2) <- inEnv newEnv $ infer e2
          let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) [i1, i2]
          return
            ( Let p1 loc (Expl x) p2 e1' p3 e2',
              ImplType isMerged t2,
              Set.fromList ics `Set.union` c1 `Set.union` c2
            )
        Let p1 loc (Impl x) p2 e1 p3 e2 -> do
          (e1', ImplType i1 t1, c1) <- infer e1
          (e2', ImplType i2 t2, c2) <- infer e2

          v1 <- case Map.lookup x i2 of
            Just t -> return t
            Nothing -> fresh

          let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) [i1, Map.withoutKeys i2 (Set.singleton x)]
              tyCls = Set.fromList $ map snd $ rights $ Set.toList $ c1 `Set.union` c2

          return
            ( Let p1 loc (Impl x) p2 e1' p3 e2',
              ImplType isMerged t2,
              Set.fromList ics
                `Set.union` c1
                `Set.union` c2
                `Set.union` Set.singleton (tyConstr v1 t1 [ImplicitVarTypeOverlap tyCls x v1 t1 $ blockPosition expr])
            )
        Op e1 loc mHash opMeta modNm op e2 -> do
          let (sPos, ePos) = elementPosition loc op
          let opLoc = (sPos, incSourceCol ePos $ fromScoped 0 $ (+ 1) . Text.length . unModuleName <$> modNm)

          (e1', ImplType i1 t1, c1) <- infer e1
          (e2', ImplType i2 t2, c2) <- infer e2

          meta <- lookupEnv opLoc (maybe (error "internal error, infix ops must always be pinned!!") Left $ pinnedToMaybe mHash)
          let (tcs, (u1, u2, u3)) = opGetTyComponents <$> ty meta

          tv <- fresh
          let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) [i1, i2]
              tyCls = Set.fromList $ map snd $ rights $ Set.toList $ c1 `Set.union` c2

          attachTypeToPosition opLoc meta {ty = (tcs, ImplType Map.empty $ t1 `TArr` (t2 `TArr` tv))}

          return
            ( Op e1' loc mHash opMeta modNm op e2',
              ImplType isMerged tv,
              Set.fromList ics
                `Set.union` c1
                `Set.union` c2
                `Set.union` Set.fromList
                  [ tyConstr u1 t1 [UnificationFail tyCls u1 t1 $ blockPosition e1],
                    tyConstr u2 t2 [UnificationFail tyCls u2 t2 $ blockPosition e2],
                    tyConstr u3 tv [UnificationFail tyCls u3 tv $ blockPosition expr]
                  ]
                `Set.union` (Set.map (Right . (opLoc,)) tcs)
            )
        PreOp loc mHash opMeta modNm op e -> do
          let (sPos, ePos) = elementPosition loc op
          let opLoc = (sPos, incSourceCol ePos $ fromScoped 0 $ (+ 1) . Text.length . unModuleName <$> modNm)

          (e', ImplType i t, c) <- infer e

          meta <- lookupEnv opLoc (maybe (error "internal error, prefix ops must always be pinned!!") Left $ pinnedToMaybe mHash)
          let (tcs, (u1, u2)) = preOpGetTyComponents <$> ty meta
              tyCls = Set.fromList $ map snd $ rights $ Set.toList $ c

          tv <- fresh
          attachTypeToPosition opLoc meta {ty = (tcs, ImplType Map.empty $ t `TArr` tv)}

          return
            ( PreOp loc mHash opMeta modNm op e',
              ImplType i tv,
              c
                `Set.union` Set.fromList
                  [ tyConstr u1 t [UnificationFail tyCls u1 t $ blockPosition e],
                    tyConstr u2 tv [UnificationFail tyCls u2 tv $ blockPosition expr]
                  ]
                `Set.union` (Set.map (Right . (opLoc,)) tcs)
            )
        If p1 cond p2 tr p3 fl -> do
          (cond', ImplType i1 t1, c1) <- infer cond
          (tr', ImplType i2 t2, c2) <- infer tr
          (fl', ImplType i3 t3, c3) <- infer fl

          let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) [i1, i2, i3]
              tyCls = Set.fromList $ map snd $ rights $ Set.toList $ c1 `Set.union` c2 `Set.union` c3

          return
            ( If p1 cond' p2 tr' p3 fl',
              ImplType isMerged t2,
              Set.fromList ics
                `Set.union` c1
                `Set.union` c2
                `Set.union` c3
                `Set.union` Set.fromList
                  [ tyConstr t1 typeBool [IfConditionMustBeBool tyCls t1 $ blockPosition cond],
                    tyConstr t2 t3 [IfBranchesMustBeEqType tyCls t2 t3 (blockPosition tr) (blockPosition fl)]
                  ]
            )
        Tuple p1 es p2 -> do
          (es', impls, tys, cs) <- go $ tListToList es
          let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) impls
          let inferredTy = ImplType isMerged $ TTuple $ tListFromList tys
          attachTypeToPosition
            exprLoc
            TypeMetadata
              { identExpr = bimap (const ()) (const ()) $ removeComments expr,
                ty = (Set.empty, inferredTy),
                docs = Nothing
              }

          return
            ( Tuple p1 (tListFromList es') p2,
              inferredTy,
              Set.fromList ics `Set.union` cs
            )
          where
            go [] = return ([], [], [], Set.empty)
            go ((e', p3) : es') = do
              (e'', ImplType i t, cs) <- infer e'
              (es'', impls, tRest, csRest) <- go es'
              return ((e'', p3) : es'', i : impls, t : tRest, cs `Set.union` csRest)
        Assert p1 cond p2 e -> do
          (cond', ImplType i1 t1, c1) <- infer cond
          (e', ImplType i2 t2, c2) <- infer e

          let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) [i1, i2]
          return
            ( Assert p1 cond' p2 e',
              ImplType isMerged t2,
              Set.fromList ics
                `Set.union` c1
                `Set.union` c2
                `Set.union` Set.singleton (tyConstr t1 typeBool [AssertConditionMustBeBool (Set.fromList . map snd . rights . Set.toList $ c1 `Set.union` c2) t1 $ blockPosition cond])
            )
        Empty _ -> do
          meta <- lookupEnv exprLoc $ Left emptyHash
          let (_, t) = ty meta
          attachTypeToPosition exprLoc meta
          return (expr, t, Set.empty)
        One p e -> do
          (e', ImplType is ty, cs) <- infer e
          meta <- lookupEnv exprLoc $ Left oneHash
          attachTypeToPosition exprLoc meta {ty = (Set.empty, ImplType is $ TOptional ty)}
          return (One p e', ImplType is $ TOptional ty, cs)
        Case p1 e p2 patExprs' p3 -> do
          let patExprs = NEList.toList patExprs'
          (e', ImplType i_e t_e, cs_e) <- infer e
          (patTys, patVars, patConstraints) <-
            unzip3
              <$> mapM
                (\p -> checkVariableOverlap Map.empty p >> mkPatConstraint p)
                (map (\(_, p, _, _) -> p) patExprs)

          addCasePatterns exprLoc $ map (\(_, p, _, _) -> p) patExprs

          res <- forM (zip patVars $ map (\(_, _p, _, e'') -> e'') patExprs) $
            \(vars, e''') -> foldr inEnv (infer e''') $ map (\(Ident x, meta) -> (ExtIdent $ Right x, meta)) vars

          let (es'', is_res, ts_res, cs_res) = unzip4 $ map (\(e'', ImplType i_r t_r, cs_r) -> (e'', i_r, t_r, cs_r)) res
              (isMerged, ics) = mergeImplicitMaps (blockPosition expr) (i_e : is_res)
              tyCls = Set.fromList $ map snd $ rights $ Set.toList $ cs_e `Set.union` (Set.unions cs_res)
              patTysEqConstraints =
                Set.fromList
                  [ tyConstr tPat4 tPat5 [PatternsMustBeEqType tyCls tPat4 tPat5 p4 p5 (blockPosition p4) (blockPosition p5)]
                    | (tPat4, p4) <- zip patTys (map (\(_, p, _, _) -> p) patExprs),
                      (tPat5, p5) <- zip patTys (map (\(_, p, _, _) -> p) patExprs),
                      p4 /= p5
                  ]
              patTysMustEqCaseExprTy cExprTy =
                Set.fromList
                  [ tyConstr tPat cExprTy [PatternUnificationFail tPat cExprTy p $ blockPosition p]
                    | (tPat, p) <- zip patTys (map (\(_, p, _, _) -> p) patExprs)
                  ]
              patExpTysEqConstraints set =
                Set.fromList
                  [ tyConstr t1 t2 [CaseBranchesMustBeEqType tyCls t1 t2 (blockPosition e1) (blockPosition e2)]
                    | (ImplType _ t1, e1) <- set,
                      (ImplType _ t2, e2) <- set,
                      e1 /= e2
                  ]

          return $
            ( Case p1 e' p2 (NEList.fromList $ map (\(e'', (p6, pat, p7, _)) -> (p6, pat, p7, e'')) $ zip es'' patExprs) p3,
              ImplType isMerged $ head ts_res,
              (Set.fromList ics)
                `Set.union` cs_e
                `Set.union` (Set.unions patConstraints)
                `Set.union` patTysEqConstraints
                `Set.union` patTysMustEqCaseExprTy t_e
                `Set.union` patExpTysEqConstraints (zip (map (\(_, ty, _) -> ty) res) (map (\(_, _p, _, e'') -> e'') patExprs))
                `Set.union` (Set.unions cs_res)
            )
          where
            mkPatConstraint :: Pat (Pinned VCObjectHash) SourcePos -> Infer (InfernoType, [(Ident, TypeMetadata TCScheme)], Set.Set Constraint)
            mkPatConstraint pat =
              let patLoc = blockPosition pat
               in case pat of
                    PVar _ (Just (Ident x)) -> do
                      tv <- fresh
                      attachTypeToPosition
                        patLoc
                        TypeMetadata
                          { identExpr = Var () () LocalScope $ Expl $ ExtIdent $ Right x,
                            ty = (Set.empty, ImplType Map.empty tv),
                            docs = Nothing
                          }
                      let meta =
                            TypeMetadata
                              { identExpr = Var () () LocalScope $ Expl $ ExtIdent $ Right x,
                                ty = ForallTC [] Set.empty $ ImplType Map.empty tv,
                                docs = Nothing
                              }
                      return (tv, [(Ident x, meta)], Set.empty)
                    PEnum _ Local _ _ -> error "internal error, malformed pattern enum must be pinned"
                    PEnum _ hash sc i -> do
                      meta <- lookupEnv patLoc $ Left $ fromJust $ pinnedToMaybe hash
                      let (_, ImplType _ t) = ty meta
                      attachTypeToPosition patLoc meta {identExpr = Enum () () sc i}
                      return (t, [], Set.empty)
                    PLit _ l ->
                      inferPatLit
                        patLoc
                        l
                        ( case l of
                            LInt _ -> typeInt
                            LDouble _ -> typeDouble
                            LHex _ -> typeWord64
                            LText _ -> typeText
                        )
                    POne _ p -> do
                      (t, vars, cs) <- mkPatConstraint p
                      meta <- lookupEnv patLoc $ Left oneHash
                      attachTypeToPosition patLoc meta {ty = (Set.empty, ImplType Map.empty $ t .-> TOptional t)}
                      return (TOptional t, vars, cs)
                    PEmpty _ -> do
                      meta <- lookupEnv patLoc $ Left emptyHash
                      let (_, ImplType _ t) = ty meta
                      attachTypeToPosition patLoc meta
                      return (t, [], Set.empty)
                    PArray _ [] _ -> do
                      tv <- fresh
                      let t = TArray tv
                      attachTypeToPosition
                        patLoc
                        TypeMetadata
                          { identExpr = patternToExpr $ bimap (const ()) (const ()) pat,
                            ty = (Set.empty, ImplType Map.empty $ t),
                            docs = Nothing
                          }
                      return (t, [], Set.empty)
                    PArray _ ((p, _) : ps) _ -> do
                      (t, vars1, csP) <- mkPatConstraint p
                      (vars2, csPs) <- aux t ps
                      let inferredTy = TArray t
                      attachTypeToPosition
                        patLoc
                        TypeMetadata
                          { identExpr = patternToExpr $ bimap (const ()) (const ()) pat,
                            ty = (Set.empty, ImplType Map.empty $ inferredTy),
                            docs = Nothing
                          }
                      return (inferredTy, vars1 ++ vars2, csP `Set.union` csPs)
                      where
                        aux _t [] = return ([], Set.empty)
                        aux t ((p', _) : ps') = do
                          (t', vars', cs') <- mkPatConstraint p'
                          (vars, cs) <- aux t ps'
                          let tIst' = tyConstr t t' [UnificationFail Set.empty t t' $ blockPosition p']
                          return
                            ( vars' ++ vars,
                              cs' `Set.union` cs `Set.union` Set.singleton tIst'
                            )
                    PTuple _ ps _ -> do
                      (ts, vars, cs) <- aux $ tListToList ps
                      let inferredTy = TTuple $ tListFromList ts
                      attachTypeToPosition
                        patLoc
                        TypeMetadata
                          { identExpr = patternToExpr $ bimap (const ()) (const ()) pat,
                            ty = (Set.empty, ImplType Map.empty $ inferredTy),
                            docs = Nothing
                          }
                      return (inferredTy, vars, cs)
                      where
                        aux [] = return ([], [], Set.empty)
                        aux ((p', _l) : ps') = do
                          (t, vars1, cs1) <- mkPatConstraint p'
                          (ts, vars2, cs2) <- aux ps'
                          return (t : ts, vars1 ++ vars2, cs1 `Set.union` cs2)
                    PVar _ Nothing -> do
                      tv <- fresh
                      let meta =
                            TypeMetadata
                              { identExpr = patternToExpr $ bimap (const ()) (const ()) pat,
                                ty = (Set.empty, ImplType Map.empty tv),
                                docs = Nothing
                              }
                      attachTypeToPosition patLoc meta
                      return (tv, [], Set.empty)
                    PCommentAbove _ p -> mkPatConstraint p
                    PCommentAfter p _ -> mkPatConstraint p
                    PCommentBelow p _ -> mkPatConstraint p

            checkVariableOverlap :: Map.Map Ident (Location SourcePos) -> Pat (Pinned VCObjectHash) SourcePos -> Infer (Map.Map Ident (Location SourcePos))
            checkVariableOverlap vars pat =
              let patLoc = blockPosition pat
               in case pat of
                    PVar _ (Just x) -> case Map.lookup x vars of
                      Just loc' -> throwError [VarMultipleOccurrence x patLoc loc']
                      Nothing -> return $ Map.insert x patLoc vars
                    POne _ p -> checkVariableOverlap vars p
                    PArray _ ps _ -> foldM checkVariableOverlap vars $ map fst ps
                    PTuple _ ps _ -> foldM checkVariableOverlap vars $ map fst $ tListToList ps
                    _ -> return vars
        CommentAbove p e -> do
          (e', ty, cs) <- infer e
          return (CommentAbove p e', ty, cs)
        CommentAfter e p -> do
          (e', ty, cs) <- infer e
          return (CommentAfter e' p, ty, cs)
        CommentBelow e p -> do
          (e', ty, cs) <- infer e
          return (CommentBelow e' p, ty, cs)
        Bracketed p1 e p2 -> do
          (e', ty, cs) <- infer e
          return (Bracketed p1 e' p2, ty, cs)
        RenameModule l1 newNm l2 oldNm l3 e -> do
          s@InferState {modules = mods} <- get
          when (newNm `Map.member` mods) $ throwError [ModuleNameTaken newNm $ elementPosition l1 newNm]
          case Map.lookup oldNm mods of
            Nothing -> throwError [ModuleDoesNotExist oldNm (l2, l3)]
            Just oldNmMod -> do
              put s {modules = Map.insert newNm oldNmMod mods}
              (e', ty, cs) <- infer e
              modify (\s' -> s' {modules = Map.delete newNm $ modules s})
              return (RenameModule l1 newNm l2 oldNm l3 e', ty, cs)
        OpenModule l1 mHash modNm@(ModuleName n) imports p e -> do
          InferState {modules = mods} <- get
          case Map.lookup modNm mods of
            Nothing -> throwError [ModuleDoesNotExist modNm $ elementPosition l1 $ Ident n]
            Just _openMod -> do
              (e', ty, cs) <- infer e
              return (OpenModule l1 mHash modNm imports p e', ty, cs)

inferPatLit :: Location SourcePos -> Lit -> InfernoType -> Infer (InfernoType, [b], Set.Set c)
inferPatLit loc n t =
  attachTypeToPosition
    loc
    TypeMetadata
      { identExpr = Lit () n,
        ty = (Set.empty, ImplType Map.empty t),
        docs = Nothing
      }
    >> return (t, [], Set.empty)

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

-- | Run the constraint solver
runSolve :: Set.Set TypeClass -> [Constraint] -> Either [TypeError SourcePos] Subst
runSolve allClasses cs = runIdentity $ runExceptT $ flip runReaderT allClasses $ solver st
  where
    st = (emptySubst, cs)

unifyMany :: [TypeError SourcePos] -> [InfernoType] -> [InfernoType] -> Solve Subst
unifyMany _ [] [] = return emptySubst
unifyMany err (t1 : ts1) (t2 : ts2) = do
  su1 <- unifies err t1 t2
  su2 <- unifyMany err (apply su1 ts1) (apply su1 ts2)
  return (su2 `compose` su1)
unifyMany err _ _ = trace "throwing in unifyMany " $ throwError err

unifies :: [TypeError SourcePos] -> InfernoType -> InfernoType -> Solve Subst
unifies _ t1 t2 | t1 == t2 = return emptySubst
unifies err (TVar v) t = bind err v t
unifies err t (TVar v) = bind err v t
unifies err (TArr t1 t2) (TArr t3 t4) = unifyMany err [t1, t2] [t3, t4]
unifies err (TArray t1) (TArray t2) = unifies err t1 t2
unifies err (TSeries t1) (TSeries t2) = unifies err t1 t2
unifies err (TOptional t1) (TOptional t2) = unifies err t1 t2
unifies err (TTuple ts1) (TTuple ts2)
  | length (tListToList ts1) == length (tListToList ts2) = unifyMany err (tListToList ts1) (tListToList ts2)
  | otherwise = throwError [UnificationFail (getTypeClassFromErrs err) (TTuple ts1) (TTuple ts2) loc | loc <- (getLocFromErrs err)]
unifies err _ _ =
  -- trace "throwing in unifies " $
  throwError err

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    _ -> do
      let (tyConstrs, typeCls) = partitionEithers cs
      su1 <- solverTyCs su tyConstrs
      let partResolvedTyCls = map (\(loc, tc) -> (loc, apply su1 tc)) typeCls
      -- trace ("partResolvedTyCls: " <> (intercalate "\n" $ map (unpack . renderPretty . pretty . snd) partResolvedTyCls)) $
      evalSolveState (solverTypeClasses $ su1 `compose` su) (Set.fromList partResolvedTyCls, mempty)

solverTyCs :: Subst -> [(InfernoType, InfernoType, [TypeError SourcePos])] -> Solve Subst
solverTyCs su cs =
  case cs of
    [] -> return su
    ((t1, t2, errs) : cs0) -> do
      su1 <- unifies errs t1 t2
      solverTyCs (su1 `compose` su) (map (\(t1', t2', es) -> (apply su1 t1', apply su1 t2', map (apply su1) es)) cs0)

evalSolveState :: SolveState st a -> st -> Solve a
evalSolveState (ReaderT f) st = ReaderT $ \r -> evalStateT (f r) st

liftToSolveState :: Solve a -> SolveState st a
liftToSolveState (ReaderT f) = ReaderT $ \r -> StateT $ \s -> (,s) <$> f r

pick :: (Show a, Ord a) => SolveState (Set.Set a, Set.Set a) (Maybe a)
pick = state $ \st@(current, marked) ->
  case Set.lookupMin current of
    Nothing -> (Nothing, st)
    Just a -> (Just a, (Set.delete a current, Set.insert a marked))

-- | `applySubsts` applies the substitution `su` on both marked and unmarked typeclasses and an new information, propagated to
--   marked typeclasses, causes said typeclass to be moved back to the "current" set.
--   We then filter out any fully resolved classes in the marked set only!! to avoid extra unnecessary steps.
--   (Filtering the unprocessed, i.e. current classes may lead to subtle bugs if the class is fully instantiated but
--   is not in fact an instance found in `allClasses`)
applySubsts :: (Ord loc) => Subst -> SolveState (Set.Set (loc, TypeClass), Set.Set (loc, TypeClass)) ()
applySubsts su = state $ \(current, marked) ->
  (\(c, m) -> ((), (c, filterFullyInstantiated m))) $
    foldr
      ( \(loc, a) (current', marked') ->
          let a' = apply su a
           in if a == a'
                then (current', Set.insert (loc, a') marked')
                else (Set.insert (loc, a') current', marked')
      )
      (Set.map (\(loc, a) -> (loc, apply su a)) current, mempty)
      marked
  where
    filterFullyInstantiated =
      Set.filter $
        not
          . Set.null
          . ftv
          . snd

solverTypeClasses :: Subst -> SolveState (Set.Set (Location SourcePos, TypeClass), Set.Set (Location SourcePos, TypeClass)) Subst
solverTypeClasses su =
  pick >>= \case
    Nothing -> return su
    Just (loc, tc@(TypeClass nm tys)) -> do
      allClasses <- ask
      let matchingInstances = Set.toList $ Set.filter (\(TypeClass nm' _) -> nm == nm') allClasses
      if null matchingInstances
        then throwError [TypeClassNotFoundError allClasses tc loc]
        else do
          res <- liftToSolveState (catMaybes <$> forM matchingInstances (tryMatchPartial tys))
          case res of
            [] -> throwError [TypeClassNoPartialMatch tc loc]
            (Subst s : xs) -> do
              -- even if we have multiple matching substitutions, we can still make progress if they all agree
              -- on some parameter
              let su' = (Subst $ foldr intersection s [x | Subst x <- xs]) `compose` su
              -- trace ("applying su': "<> show su' <> "\nprevious was su: " <> show su) $
              applySubsts su'
              solverTypeClasses su'
  where
    intersection = Map.merge Map.dropMissing Map.dropMissing (Map.zipWithMaybeMatched $ \_ a b -> if a == b then Just a else Nothing)

newtype Counter = Counter Int

-- | Use `getLit` if you want to remember what it points to, i.e. if mapping a (TVar, InfernoType) to a SAT solver variable
getLit :: (MonadState s m, HasType (Bimap.Bimap Int a) s, HasType Counter s, Ord a) => a -> m Int
getLit a = do
  st <- get
  let bm = getTyped st
  let Counter i = getTyped st
  case Bimap.lookupR a bm of
    Just l -> pure l
    Nothing -> do
      put $ setTyped (Counter $ i + 1) $ setTyped (Bimap.insert i a bm) st
      pure i

-- | Return a fresh SAT solver variable
newLit :: (MonadState s m, HasType Counter s) => m Int
newLit = do
  st <- get
  let Counter i = getTyped st
  put $ setTyped (Counter $ i + 1) st
  pure i

addClause :: (MonadState s m, HasType [[Int]] s) => [Int] -> m ()
addClause c = do
  st <- get
  let clauses = getTyped st
  put $ setTyped (c : clauses) st

-- | This function encodes our typeclasses into CNF clauses for the SAT-solver.
-- The translation works in the following way:
-- Given a set of type-classes, e.g. `{requires addition on 'a int producing 'b ,requires addition on 'b 'b producing double}`
-- we first compute all the matching instances from `allClasses`, in this case, we get:
-- `requires addition on 'a int producing 'b` matches:
-- - `requires addition on int int producing int`
-- - `requires addition on double int producing double`
-- `requires addition on 'b 'b producing double` matches:
-- - `requires addition on double double producing double`
-- Now we translate each possible class instantiation into the following clauses:
-- `requires addition on int int producing int` becomes:
-- `'a_int <-> add_class_1_inst_1_arg_1`
-- `'b_int <-> add_class_1_inst_1_arg_3`
-- `add_class_1_inst_1_arg_1 /\ add_class_1_inst_1_arg_3 <-> add_class_1_inst_1`
-- `requires addition on double int producing double` becomes:
-- `'a_double <-> add_class_1_inst_2_arg_1`
-- `'b_doube <-> add_class_1_inst_2_arg_3`
-- `add_class_1_inst_2_arg_1 /\ add_class_1_inst_2_arg_3 <-> add_class_1_inst_2`
-- We have to make sure that exactly one instance matches,
-- i.e. `requires addition on int int producing int` or `requires addition on double int producing double`, but not both:
-- `add_class_1_inst_1 XOR add_class_1_inst_2`
-- Next we encode the second set of matching instances, namely `requires addition on double double producing double`:
-- `'b_doube <-> add_class_2_inst_1_arg_1`
-- `'b_doube <-> add_class_2_inst_1_arg_2`
-- `add_class_2_inst_1_arg_1 /\ add_class_2_inst_1_arg_2 <-> add_class_2_inst_1`
-- Since the second typeclass only matches one instance, we simply add it in as true:
-- `add_class_2_inst_1`
-- Finally, we collect all the variables with all their possible types and encode the condition that exactly one type for each is matched:
-- `'a_int XOR 'a_double`
-- `'b_int XOR 'b_double`
-- If the SAT solver returns SAT, we simply check which one of `'a_?` and `'b_?` is set to true in the resulting model.
-- If the solver returns the model `'a_double /\ 'b_double` and we want to check for any more solutions, we simply add `-('a_double /\ 'b_double)`
-- (`-'a_double \/ -'b_double` in CNF) as a clause and re-run the solver. Once all assignments have been exhausted, we will get UNSAT.
{-# NOINLINE findTypeClassWitnesses #-}
findTypeClassWitnesses :: Set.Set TypeClass -> Maybe Int -> Set.Set TypeClass -> Set.Set TV -> [Subst]
findTypeClassWitnesses allClasses iters tyCls tvs =
  unsafePerformIO $
    Picosat.evalScopedPicosat $
      Picosat.addBaseClauses clauses >> getSolutions iters
  where
    filteredSubs = filteredTypeClassSubstitutions allClasses $ Set.toList tyCls
    (_, litMap, clauses) = flip execState (Counter 1, Bimap.empty, []) $ do
      encodeTypeClasses allClasses filteredSubs $ Set.toList tyCls
      lm :: Bimap.Bimap Int (TV, InfernoType) <- getTyped <$> get
      let ls_grouped = foldr (\(l, (tv, _)) m' -> Map.alter (Just . maybe [l] (l :)) tv m') mempty $ Bimap.toList $ lm
      forM_ (Map.elems ls_grouped) $ \ls -> xor ls

    getSolutions = \case
      Just 0 -> pure []
      i -> do
        Picosat.scopedSolutionWithAssumptions [] >>= \case
          Picosat.Solution ls -> do
            let found = catMaybes $ map (\l -> (l,) <$> Bimap.lookup l litMap) ls
            Picosat.addBaseClauses [[-l | (l, (tv, _)) <- found, tv `Set.member` tvs]]
            ((Subst $ Map.fromList $ map snd found) :) <$> getSolutions ((\x -> x - 1) <$> i)
          _ -> pure []

tryMatchPartial :: [InfernoType] -> TypeClass -> Solve (Maybe Subst)
tryMatchPartial tys (TypeClass _ tys2) =
  ((Just <$> unifyMany [] tys tys2) `catchError` (\_ -> return Nothing))

-- | This is a minor optimisation for the `encodeTypeClasses` function. The `filteredTypeClassSubstitutions` function takes the set of all type class instances,
-- along with the list of all the current classes we want to unify and computes all the matching substitutions.
-- It then recursively merges all the possible substitutions for each class and intersects the merged maps of each class instance.
-- For example, given `{requires addition on 'a 'b producing 'c, requires multiplication on 'c 'd producing 'e}`, we will first compute the merged substitutions
-- `{'a -> {int, double, time, timeDiff, word16, word32, word64}, 'b -> {int, double, time, timeDiff, word16, word32, word64}, 'c -> {int, double, time, timeDiff, word16, word32, word64}}`
-- for the `addition` typeclass, and
-- `{'c -> {int, double}, 'd -> {int, double}, 'e -> {int, double}}` for the `multiplication` one.
-- Then we merge the two maps and obtain
-- `{'a -> {int, double, time, timeDiff, word16, word32, word64}, 'b -> {int, double, time, timeDiff, word16, word32, word64}, 'c -> {int, double}, 'd -> {int, double}, 'e -> {int, double}}`
-- This map represents the set of all possible "consistent" type assingments for the free variables `'a`,`'b`,..,`'e`, such that there may exist matching instances of both typeclasses.
-- Notice that `'c` can only an `int` or a `double`, because there are no typeclass intances of `multiplication` for any other types. Precomputing these constraints reduces the number of
-- SAT solver clauses somewhat, especially for large arithmetic operations, e.g. `fun a b c d e f g h i j -> a + b * (c - d) + e / f / g * (h + i - j)`
filteredTypeClassSubstitutions :: Set.Set TypeClass -> [TypeClass] -> Map.Map TV (Set.Set InfernoType)
filteredTypeClassSubstitutions allClasses = \case
  [] -> mempty
  TypeClass nm tys : tcs -> do
    let possibleMatchingInstances = Set.toList $ Set.filter (\(TypeClass nm' _) -> nm == nm') allClasses
    case runIdentity $ runExceptT $ flip runReaderT allClasses $ (catMaybes <$> forM possibleMatchingInstances (tryMatchPartial tys)) of
      Left _ -> filteredTypeClassSubstitutions allClasses tcs
      Right subs' ->
        let subs = [su | Subst su <- subs']
            mergedSubs = foldr (Map.merge (Map.mapMissing $ \_k a -> Set.singleton a) Map.preserveMissing $ Map.zipWithMatched $ \_k a as -> Set.insert a as) mempty subs
            finalMap = filteredTypeClassSubstitutions allClasses tcs
         in Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched $ \_k as bs -> Set.intersection as bs) mergedSubs finalMap

encodeTypeClasses ::
  (MonadState s f, HasType (Bimap.Bimap Int (TV, InfernoType)) s, HasType [[Int]] s, HasType Counter s) =>
  Set.Set TypeClass ->
  Map.Map TV (Set.Set InfernoType) ->
  [TypeClass] ->
  f ()
encodeTypeClasses allClasses filteredSubs = \case
  [] -> pure ()
  TypeClass nm tys : tcs -> do
    let possibleMatchingInstances = Set.toList $ Set.filter (\(TypeClass nm' _) -> nm == nm') allClasses
    case runIdentity $ runExceptT $ flip runReaderT allClasses $ (catMaybes <$> forM possibleMatchingInstances (tryMatchPartial tys)) of
      Left _err -> encodeTypeClasses allClasses filteredSubs tcs
      Right subs -> do
        insts <- forM (filterSubs subs) $ \(Subst su) -> do
          ls <-
            concat
              <$> ( forM tys $ \t ->
                      case t of
                        TVar tv -> do
                          let t' = su Map.! tv
                          tvLit <- getLit (tv, t')
                          freshLit <- newLit
                          [tvLit] `iff` freshLit
                          pure [freshLit]
                        _ -> pure []
                  )
          freshLit <- newLit
          ls `iff` freshLit
          pure freshLit
        xor insts
        encodeTypeClasses allClasses filteredSubs tcs
  where
    filterSubs =
      filter $ \(Subst su) ->
        Map.foldrWithKey
          ( \k v cond ->
              cond && case Map.lookup k filteredSubs of
                Nothing -> False
                Just vs -> Set.member v vs
          )
          True
          su

    -- a_1 /\ ... /\ a_n -> b is equivalent to -a_1 \/ ... \/ -a_n \/ b
    impl as b = addClause $ b : map (\a -> -a) as
    -- a_1 /\ ... /\ a_n <-> b is equivalent to (a_1 /\ ... /\ a_n -> b) /\ (b -> a_1) /\ ... /\ (b -> a_n)
    iff as b = do
      as `impl` b
      forM_ as $ \a -> [b] `impl` a

xor :: (MonadState s m, HasType [[Int]] s) => [Int] -> m ()
xor ls =
  do
    addClause ls
    go ls
  where
    go [] = pure ()
    go (x : xs) = do
      forM_ xs $ \y -> addClause [-x, -y]
      go xs

bind :: [TypeError SourcePos] -> TV -> InfernoType -> Solve Subst
bind err a t
  | t == TVar a = return emptySubst
  | occursCheck a t = throwError [InfiniteType a t loc | loc <- (getLocFromErrs err)]
  | otherwise = return (Subst $ Map.singleton a t)

occursCheck :: (Substitutable a) => TV -> a -> Bool
occursCheck a t = a `Set.member` ftv t
