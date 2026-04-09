{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
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

import Control.Monad.Except
  ( Except,
    ExceptT,
  )
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.Foldable (foldl')
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Set as Set
import GHC.Records (HasField (getField))
import Inferno.Infer.Env (Env, TypeMetadata, closeOver, closeOverType)
import Inferno.Infer.Error (TypeError)
import Inferno.Types.Module (Module, PinnedModule)
import Inferno.Types.Syntax
  ( Expr,
    ExtIdent,
    Ident,
    ModuleName,
    Pat,
    RestOfRecord,
  )
import Inferno.Types.Type
  ( ImplType (ImplType),
    InfernoType,
    Subst,
    Substitutable (apply, ftv),
    TCScheme,
    TV,
    TypeClass (TypeClass),
  )
import Inferno.Types.VersionControl (Pinned (..), VCObjectHash)
import Prettyprinter (Pretty (pretty), (<+>))
import Text.Megaparsec (SourcePos)

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
  { count :: !Int
  , typeMap :: !(Map.Map (Location SourcePos) (TypeMetadata (Set.Set TypeClass, ImplType)))
  , modules :: !(Map.Map ModuleName (Module ()))
  , typeClasses :: !(Set.Set TypeClass)
  , patternsToCheck :: ![(Location SourcePos, [Pat (Pinned VCObjectHash) SourcePos])]
  }

-- | Initial inference state
-- initInfer :: InferState
-- initInfer = InferState{count = 0, typeMap = Map.empty, modules = Map.empty, typeClasses = Set.empty, patternsToCheck = []}

type Constraint = Either (InfernoType, InfernoType, [TypeError SourcePos]) (Location SourcePos, TypeClass)

type Unifier = (Subst, [Constraint])

-- | Constraint solver monad
type Solve a = ReaderT (Set.Set TypeClass) (ExceptT [TypeError SourcePos] Identity) a

type SolveState st a = ReaderT (Set.Set TypeClass) (StateT st (ExceptT [TypeError SourcePos] Identity)) a

type Location a = (a, a)

-- | Result of inferring a sub-expression: the elaborated expression,
-- its implicit type, and the generated constraints.
data InferResult = InferResult
  { expr :: Expr (Pinned VCObjectHash) SourcePos
  , typ :: ImplType
  , constrs :: Seq Constraint
  }

-- | A single branch of a @match@ expression; local record to avoid
-- repeatedly destructuring the raw 4-tuple from the AST.
data CaseBranch = CaseBranch
  { barPos :: SourcePos
  , pat :: Pat (Pinned VCObjectHash) SourcePos
  , arrPos :: SourcePos
  , body :: Expr (Pinned VCObjectHash) SourcePos
  }

-- | Virtual record fields for `ImplType`, which has positional arguments. This
-- makes it much easier to access the relevant components when recursively calling
-- @infer@
instance HasField "impl" ImplType (Map.Map ExtIdent InfernoType) where
  getField (ImplType m _) = m

instance HasField "body" ImplType InfernoType where
  getField (ImplType _ t) = t

instance Substitutable Constraint where
  apply s (Left (t1, t2, es)) = Left (apply s t1, apply s t2, es)
  apply s (Right (loc, tc)) = Right (loc, apply s tc)
  ftv (Left (t1, t2, _)) = ftv t1 `Set.union` ftv t2
  ftv (Right (_, tc)) = ftv tc

instance Pretty Constraint where
  pretty (Left (t1, t2, _errs)) = pretty t1 <+> "~" <+> pretty t2
  pretty (Right (_loc, tc)) = pretty tc

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- filterInstantiatedTypeClasses :: Set.Set TypeClass -> Set.Set TypeClass
-- filterInstantiatedTypeClasses = Set.filter $ not . Set.null . ftv

-- mkPattern :: Pat (Pinned VCObjectHash) SourcePos -> Pattern
-- mkPattern = \case
--   PVar _ _ -> W
--   PEnum _ Local _ns _ident -> error "internal error. cannot convert unpinned enum into a pattern"
--   PEnum _ hash _ns ident@(Ident i) -> cEnum (vcHash (ident, fromJust $ pinnedToMaybe hash)) i
--   PLit _ l -> case l of
--     LInt v -> cInf v
--     LDouble v -> cInf v
--     LHex v -> cInf v
--     LText v -> cInf $ mkEnumText v
--   POne _ p -> cOne $ mkPattern p
--   PEmpty _ -> cEmpty
--   PArray _ ps _ -> cInf $ mkEnumArrayPat ps
--   PTuple _ ps _ -> cTuple $ map (mkPattern . fst) $ tListToList ps
--   PRecord _ ps _ -> cRecord (Set.fromList fs) $ map mkPattern ps'
--     where
--       (fs, ps', _) = unzip3 $ sortOn fst3 ps
--   PCommentAbove _ p -> mkPattern p
--   PCommentAfter p _ -> mkPattern p
--   PCommentBelow p _ -> mkPattern p

-- checkExhaustivenessAndUsefullness :: Map.Map VCObjectHash (Set.Set (VCObjectHash, Text.Text)) -> Location SourcePos -> [Pat (Pinned VCObjectHash) SourcePos] -> [TypeError SourcePos]
-- checkExhaustivenessAndUsefullness enum_sigs loc patts =
--   let patternsOfPatts = map ((: []) . mkPattern) patts
--    in case exhaustive enum_sigs patternsOfPatts of
--         Just ps -> [NonExhaustivePatternMatch p loc | p <- ps]
--         Nothing ->
--           let uErrs = checkUsefullness enum_sigs patternsOfPatts
--            in map
--                 ( \(i, j) ->
--                     let loc' = blockPosition $ patts !! i
--                      in let pat = patts !! j
--                          in if i == j then UselessPattern Nothing loc' else UselessPattern (Just pat) loc'
--                 )
--                 uErrs

-- | Run the inference monad
-- runInfer :: Env -> Set.Set TypeClass -> Map.Map ModuleName (Module m) -> Infer r -> Either [TypeError SourcePos] (r, InferState)
-- runInfer env allClasses allModules m =
--   runExcept $
--     runStateT
--       (runReaderT m env)
--       initInfer
--         { modules = Map.map (\m' -> m'{moduleObjects = ()}) allModules
--         , typeClasses = allClasses
--         }

-- Given a map of implicit types containing `rep of <ty>` variables, and an expression `e`
-- we want to either substitute any implicit variable `?var$n : rep of <ty>` with a `RuntimeRep <ty>`,
-- provided that `<ty>` contains no free variables
-- otherwise we want to create a closure `fun var$n -> e[var$n/?var$n]`
closeOverTypeReps :: Map.Map ExtIdent InfernoType -> Expr (Pinned VCObjectHash) SourcePos -> (Maybe TypeClass, Map.Map ExtIdent InfernoType, Expr (Pinned VCObjectHash) SourcePos)
-- closeOverTypeReps implTys expr =
closeOverTypeReps  = undefined
  -- let (tReps, rest) = flip Map.partition implTys $ \case
  --       TRep _ -> True
  --       _ -> False
  --  in if Map.null tReps
  --       then (Nothing, implTys, expr)
  --       else
  --         let
  --           -- we partition the types to those that have been fully inferred and ones where we are lacking the type-rep information
  --           (fullyInstantiated, withTypeHole) = Map.partition (Set.null . ftv) tReps
  --           -- next, we create a Map Int (Either Int InfernoType) where all the fully instantiated `TRep ty`s get mapped to a `Right ty`
  --           fullyInstantiatedMap =
  --             Map.foldrWithKey
  --               ( \k v m -> case (k, v) of
  --                   (ExtIdent (Left i), TRep ty) -> Map.insert i (Right ty) m
  --                   _ -> m
  --               )
  --               mempty
  --               fullyInstantiated

  --           -- we group all the types with holes
  --           withTypeHoleGrouped = NEList.groupBy (\(_, ty1) (_, ty2) -> ty1 == ty2) $ Map.toList withTypeHole
  --           -- each [(var$i_1, _),...,(var$i_n, _)] gets turned into the map {i_1 -> Left i_1, ... i_n -> Left i_1}
  --           substMap =
  --             foldr
  --               ( \vs m -> case NEList.head vs of
  --                   (ExtIdent (Left i), _) ->
  --                     foldr
  --                       ( \(v, _) m' -> case v of
  --                           ExtIdent (Left j) -> Map.insert j (Left i) m'
  --                           _ -> m'
  --                       )
  --                       m
  --                       vs
  --                   _ -> m
  --               )
  --               fullyInstantiatedMap
  --               withTypeHoleGrouped

  --           (pos, _) = blockPosition expr
  --           expr' = substInternalIdents substMap expr
  --          in
  --           case Map.toList withTypeHole of
  --             [] -> (Nothing, rest, expr')
  --             -- if we have any type holes, we need to wrap the expression in a lambda and add a `requires rep on ...` typeclass
  --             -- capturing all the runtime reps that the expression needs
  --             _ : _ ->
  --               let lamList = ((\merged -> let (v, _) = NEList.head merged in (pos, Just v)) <$> NEList.fromList withTypeHoleGrouped)
  --                in ( Just $
  --                       TypeClass "rep" $
  --                         map
  --                           ( \((_, ty) NEList.:| _) -> case ty of
  --                               TRep ty' -> ty'
  --                               _ -> ty
  --                           )
  --                           withTypeHoleGrouped
  --                   , rest
  --                   , Lam pos lamList pos expr'
  --                   )

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
inferExpr allModules expr = undefined
  -- let (env, inScopeClasses) = openBuiltinModuleAndAddPinnedTypes allModules
  --  in case runInfer env inScopeClasses allModules (infer expr) of
  --       -- if we threw errors whilst inferring, rethrow
  --       Left err -> Left err
  --       Right (r, InferState{..}) ->
  --         -- trace ("\ninferExpr: " <> (Text.unpack . renderPretty) r.expr)
  --         --   $ trace
  --         --     ( "ty: "
  --         --         <> (Text.unpack . renderPretty) r.typ
  --         --         <> "\ncs: "
  --         --         <> (intercalate "\n" $ map (Text.unpack . renderPretty) $ Set.toList r.constrs)
  --         --         <> "\n"
  --         --     )
  --         --   $
  --         -- case runSolve typeClasses $ filter (\case { Right (_, TypeClass "rep" _) -> False; _ -> True }) $ Set.toList r.constrs of
  --         case runSolve count typeClasses $ toList r.constrs of
  --           Left errs -> Left errs
  --           Right subst ->
  --             -- trace ("substs: " <> show subst) $
  --             -- trace ("patternsToCheck: " <> show patternsToCheck) $
  --             case concatMap (uncurry $ checkExhaustivenessAndUsefullness enumSigs) patternsToCheck of
  --               errs@(_ : _) -> Left errs
  --               _ -> do
  --                 -- get type classes and type from solved constraints
  --                 let cls = filterInstantiatedTypeClasses $ Set.map (apply subst . snd) . Set.fromList . rights $ toList r.constrs
  --                 let substitutedTy = apply subst r.typ
  --                 -- get current type variables
  --                 let tvs = ftv substitutedTy `Set.union` Set.unions (Set.elems $ Set.map ftv cls)
  --                 let res substNew (mRepTyCls, implTys', expr'') =
  --                       let finalTy =
  --                             closeOver
  --                               (filterInstantiatedTypeClasses (Set.map (apply substNew) cls) `Set.union` maybe mempty Set.singleton mRepTyCls)
  --                               $ apply substNew
  --                               $ ImplType implTys' substitutedTy.body
  --                        in Right
  --                             ( expr''
  --                             , finalTy
  --                             , Map.map
  --                                 (\meta@TypeMetadata{ty = (tcs, t)} -> meta{ty = closeOver (filterInstantiatedTypeClasses $ Set.map (apply $ substNew <> subst) tcs) $ apply (substNew <> subst) t})
  --                                 typeMap
  --                             )

  --                 if -- trace ("type classes: " <> show cls) $
  --                 Set.null cls
  --                   then res mempty $ closeOverTypeReps substitutedTy.impl r.expr
  --                   else case findTypeClassWitnesses typeClasses (Just 2) cls tvs of
  --                     [] -> Left [CouldNotFindTypeclassWitness cls $ blockPosition expr]
  --                     -- we attempt to find two type assignments. If there is only one satisfying assignment for all the type-classes, we automatically substitute the instantiations
  --                     [subst'] -> res subst' $ closeOverTypeReps (Map.map (apply subst') substitutedTy.impl) r.expr
  --                     -- even if there isn't a unique solution, we can still safely apply the substitutions to any types which do not transitively depend on the input or output type variables
  --                     -- e.g. for `let x = 3.2 in x + 2` it does not matter whether the type of `2` is an int or a double, because the final type of the whole expression won't change
  --                     (Subst s) : _ ->
  --                       let ftvsDependentOnOuterType =
  --                             Set.foldl
  --                               ( \ftvsTransClosure c ->
  --                                   let ftvCls = ftv c
  --                                    in if Set.null $ ftvCls `Set.intersection` ftvsTransClosure
  --                                         then ftvsTransClosure
  --                                         else ftvCls `Set.union` ftvsTransClosure
  --                               )
  --                               -- start with the body of the type, i.e. in `forall a_1 ... a_n {requires ..., implicit ...} => t` get the type variables in `t`
  --                               -- as well as any implicit arguments which aren't internal, since those are used for tracking type-reps
  --                               (ftv substitutedTy.body `Set.union` Map.foldrWithKey (\(ExtIdent ident) t ftvs -> case ident of Left _ -> ftvs; Right _ -> ftv t `Set.union` ftvs) mempty substitutedTy.impl)
  --                               cls
  --                           subst' = Subst $ Set.foldr Map.delete s ftvsDependentOnOuterType
  --                        in -- trace ("type ftvsDependentOnOuterType: " <> show ftvsDependentOnOuterType) $
  --                           res subst' $ closeOverTypeReps (Map.map (apply subst') substitutedTy.impl) r.expr
  -- where
  --   enumSigs :: Map.Map VCObjectHash (Set.Set (VCObjectHash, Text.Text))
  --   enumSigs =
  --     Map.foldrWithKey
  --       ( \h (ForallTC _ _ (ImplType _ ty)) m -> case ty of
  --           TBase (TEnum _n cs) ->
  --             let allConstrHashes = Set.map (\c@(Ident i) -> (vcHash (c, h), i)) cs
  --              in Map.fromList [(cH, allConstrHashes) | (cH, _) <- Set.toList allConstrHashes] `Map.union` m
  --           _ -> m
  --       )
  --       mempty
  --       $ Map.map ty
  --       $ Map.unions
  --       $ pinnedModuleHashToTy builtinModule : map pinnedModuleHashToTy (Map.elems allModules)
  --   openBuiltinModuleAndAddPinnedTypes :: Map.Map ModuleName (PinnedModule m) -> (Env, Set.Set TypeClass)
  --   openBuiltinModuleAndAddPinnedTypes modules =
  --     let Module{moduleTypeClasses = tyCls, moduleObjects = (_, tys, _)} = builtinModule
  --      in ( Env.empty
  --             { Env.pinnedTypes = Map.unions $ tys : [pinnedModuleHashToTy m | m <- Map.elems modules]
  --             }
  --         , tyCls `Set.union` Set.unions [tc | Module{moduleTypeClasses = tc} <- Map.elems modules]
  --         )

-- maxTVarInConstraints :: [Either (InfernoType, InfernoType, [a]) ((SourcePos, SourcePos), TypeClass)] -> Int
-- maxTVarInConstraints cs = case Set.lookupMax $ Set.unions $ map freeTypeVars cs of
--   Just (TV i) -> i + 1
--   Nothing -> 0
--   where
--     freeTypeVars (Right (_, c)) = ftv c
--     freeTypeVars (Left (t1, t2, _)) = ftv t1 `Set.union` ftv t2

-- | Given a type signature and some concrete assignment of types (assumes inputTys and outputTy have no free variables)
--   this function computes the runtime reps
inferTypeReps :: Set.Set TypeClass -> TCScheme -> [InfernoType] -> InfernoType -> Either [TypeError SourcePos] [InfernoType]
-- inferTypeReps allTypeClasses (ForallTC tvs tyCls (ImplType _impl ty)) inputTys outputTy
inferTypeReps = undefined
  -- let cs =
  --       [Right (dummyPos, c) | c@(TypeClass nm _) <- Set.toList tyCls, nm /= "rep"]
  --         ++ mkConstraints ty inputTys
  --     varCount = maxTVarInConstraints cs
  --  in case runSolve varCount allTypeClasses cs of
  --       Left errs -> Left errs
  --       Right subst ->
  --         let tyClsSubst = Set.map (apply subst) tyCls
  --          in case find (\(TypeClass nm _) -> nm == "rep") $ Set.toList tyClsSubst of
  --               Nothing -> pure []
  --               Just rep@(TypeClass _ runtimeRepTys) ->
  --                 if Set.null $ ftv rep
  --                   then pure runtimeRepTys
  --                   else case findTypeClassWitnesses
  --                     allTypeClasses
  --                     (Just 1)
  --                     (Set.filter (\case TypeClass "rep" _ -> False; _ -> True) tyClsSubst)
  --                     (Set.fromList tvs) of
  --                     [] -> Left [CouldNotFindTypeclassWitness tyClsSubst dummyPos]
  --                     subst' : _ -> pure $ apply subst' runtimeRepTys
  -- where
  --   mkConstraints (TArr t1 t2) (x : xs) = Left (t1, x, []) : mkConstraints t2 xs
  --   mkConstraints t [] = [Left (t, outputTy, [])]
  --   mkConstraints _ _ = error "mkConstraints: invalid input params length"
  --   dummyPos = let pos = initialPos "" in (pos, pos)

inferPossibleTypes ::
  Set.Set TypeClass ->
  TCScheme ->
  [Maybe InfernoType] ->
  Maybe InfernoType ->
  Either [TypeError SourcePos] ([[InfernoType]], [InfernoType])
-- inferPossibleTypes allTypeClasses (ForallTC _ tyCls (ImplType _impl ty)) inputTys outputTy
inferPossibleTypes = undefined
  -- let cs =
  --       [Right (dummyPos, c) | c@(TypeClass nm _) <- Set.toList tyCls, nm /= "rep"]
  --         ++ mkMaybeConstraints ty inputTys
  --     varCount = maxTVarInConstraints cs
  --  in case runSolve varCount allTypeClasses cs of
  --       Left errs -> Left errs
  --       Right subst -> do
  --         let tyClsSubst = Set.map (apply subst) $ Set.filter (\case TypeClass "rep" _ -> False; _ -> True) tyCls
  --         let (inTysFromSig, outTyFromSig) = gatherArgs $ apply subst ty
  --         let findAllPossibleTypes (supplied, t) = case supplied of
  --               Just t' -> pure [t']
  --               Nothing ->
  --                 let tvs = ftv t
  --                  in if Set.null tvs
  --                       then pure [t]
  --                       else case findTypeClassWitnesses allTypeClasses (Just 100) tyClsSubst tvs of
  --                         [] -> Left [CouldNotFindTypeclassWitness tyClsSubst dummyPos]
  --                         substs -> pure [apply sub t | sub <- substs]

  --         possibleInTysFromSig <- forM (zip inputTys inTysFromSig) findAllPossibleTypes
  --         (possibleInTysFromSig,) <$> findAllPossibleTypes (outputTy, outTyFromSig)
  -- where
  --   gatherArgs (TArr t1 t2) = first (t1 :) $ gatherArgs t2
  --   gatherArgs x = ([], x)
  --   mkMaybeConstraints (TArr t1 t2) (Just x : xs) = Left (t1, x, []) : mkMaybeConstraints t2 xs
  --   mkMaybeConstraints (TArr _ t2) (Nothing : xs) = mkMaybeConstraints t2 xs
  --   mkMaybeConstraints t [] = case outputTy of
  --     Just t' -> [Left (t, t', [])]
  --     Nothing -> []
  --   mkMaybeConstraints _ _ = error "mkConstraints: invalid input params length"
  --   dummyPos = let pos = initialPos "" in (pos, pos)

-- | Extend type environment
-- inEnv :: (ExtIdent, TypeMetadata TCScheme) -> Infer a -> Infer a
-- inEnv (x, meta) m = do
--   let scope e = Env.remove e x `Env.extend` (x, meta)
--   local scope m

-- | Lookup type in the environment
-- lookupEnv :: Location SourcePos -> Either VCObjectHash ExtIdent -> Infer (TypeMetadata (Set.Set TypeClass, ImplType))
-- lookupEnv loc x = do
--   env <- ask
--   case either (`Env.lookupPinned` env) (`Env.lookup` env) x of
--     Nothing ->
--       throwError
--         [ either
--             (\hsh -> UnboundNameInNamespace LocalScope (Left hsh) loc)
--             (\i -> UnboundExtIdent LocalScope i loc)
--             x
--         ]
--     Just meta -> do
--       iTy <- instantiate $ ty meta
--       return meta{ty = iTy}

-- mergeImplicitMaps :: Location SourcePos -> [Map.Map ExtIdent InfernoType] -> (Map.Map ExtIdent InfernoType, [Constraint])
-- mergeImplicitMaps loc =
--   foldr
--     ( \m (mAll, cs) ->
--         let cs' = Map.elems $ Map.intersectionWithKey (\ident t1 t2 -> tyConstr t1 t2 [ImplicitVarTypeOverlap mempty ident t1 t2 loc]) mAll m
--          in (mAll `Map.union` m, cs' ++ cs)
--     )
--     (Map.empty, [])

-- fresh :: Infer InfernoType
-- fresh = do
--   s@InferState{..} <- get
--   put s{count = count + 1}
--   return $ var count

-- freshRaw :: Infer Int
-- freshRaw = do
--   s@InferState{..} <- get
--   put s{count = count + 1}
--   return count

-- attachTypeToPosition :: Location SourcePos -> TypeMetadata (Set.Set TypeClass, ImplType) -> Infer ()
-- attachTypeToPosition k meta =
--   modify $ \s -> s{typeMap = Map.insert k meta s.typeMap}

-- addCasePatterns :: Location SourcePos -> [Pat (Pinned VCObjectHash) SourcePos] -> Infer ()
-- addCasePatterns k pttrns =
--   modify $
--     \s ->
--       s
--         { patternsToCheck = (k, pttrns) : s.patternsToCheck
--         }

-- instantiate :: TCScheme -> Infer (Set.Set TypeClass, ImplType)
-- instantiate (ForallTC as tcs t) = do
--   as' <- mapM (const fresh) as
--   let s = Subst $ Map.fromList $ zip as as'
--   return (Set.map (apply s) tcs, apply s t)

-- opGetTyComponents :: ImplType -> (InfernoType, InfernoType, InfernoType)
-- opGetTyComponents (ImplType _ (t1 `TArr` (t2 `TArr` t3))) = (t1, t2, t3)
-- opGetTyComponents _ = error "Invalid op type signature"

-- preOpGetTyComponents :: ImplType -> (InfernoType, InfernoType)
-- preOpGetTyComponents (ImplType _ (t1 `TArr` t2)) = (t1, t2)
-- preOpGetTyComponents _ = error "Invalid pre-op type signature"

-- tyConstr :: InfernoType -> InfernoType -> [TypeError SourcePos] -> Constraint
-- tyConstr t1 t2 es = Left (t1, t2, es)

-- inferLit :: Expr (Pinned VCObjectHash) SourcePos -> Location SourcePos -> Lit -> InfernoType -> Infer InferResult
-- inferLit expr loc l t = do
--   attachTypeToPosition loc $
--     TypeMetadata
--       { identExpr = Lit () l
--       , ty = (Set.empty, ImplType Map.empty t)
--       , docs = Nothing
--       }
--   pure InferResult{expr = expr, typ = ImplType Map.empty t, constrs = Sequence.empty}

infer :: Expr (Pinned VCObjectHash) SourcePos -> Infer InferResult
-- infer expr =
infer = undefined
--   let exprLoc = blockPosition expr
--    in case expr of
--         Lit pos l@(LInt _) -> do
--           tv <- fresh
--           let tyCls = TypeClass "numeric" [tv]
--           attachTypeToPosition exprLoc $
--             TypeMetadata
--               { identExpr = Lit () l
--               , ty = (Set.singleton tyCls, ImplType Map.empty tv)
--               , docs = Nothing
--               }

--           i <- ExtIdent . Left <$> freshRaw
--           pure
--             InferResult
--               { expr = App expr (Var pos Local LocalScope $ Impl i)
--               , typ = ImplType (Map.fromList [(i, TRep tv)]) tv
--               , constrs = Sequence.singleton $ Right (exprLoc, tyCls)
--               }
--         Lit _ l ->
--           inferLit
--             expr
--             exprLoc
--             l
--             (handleLit l)
--           where
--             handleLit (LDouble _) = typeDouble
--             handleLit (LHex _) = typeWord64
--             handleLit (LText _) = typeText
--             handleLit (LInt _) = undefined
--         Var pos mHash _modNm (Expl x) -> do
--           meta <- lookupEnv exprLoc (maybe (Right x) Left $ pinnedToMaybe mHash)
--           let (tcs, t@(ImplType impl t'')) = ty meta
--           attachTypeToPosition exprLoc meta
--           (expr', t') <- case find (\(TypeClass nm _) -> nm == "rep") $ Set.toList tcs of
--             Just (TypeClass _ runtimeRepTys) -> do
--               implRepTyps <- forM runtimeRepTys $ \repTy -> do
--                 i <- freshRaw
--                 pure (ExtIdent $ Left i, TRep repTy)
--               let (vars, _) = unzip implRepTyps

--               pure (foldl' App expr $ map (Var pos Local LocalScope . Impl) vars, ImplType (impl `Map.union` Map.fromList implRepTyps) t'')
--             Nothing -> pure (expr, t)
--           pure
--             InferResult
--               { expr = expr'
--               , typ = t'
--               , constrs = foldMap (Sequence.singleton . Right . (exprLoc,)) $ Set.filter (\case TypeClass "rep" _ -> False; _ -> True) tcs
--               }
--         Var _ _ _ (Impl x) -> do
--           tv <- fresh
--           attachTypeToPosition
--             exprLoc
--             TypeMetadata
--               { identExpr = bimap (const ()) (const ()) expr
--               , ty = (Set.empty, ImplType (Map.fromList [(x, tv)]) tv)
--               , docs = Nothing
--               }
--           pure InferResult{expr = expr, typ = ImplType (Map.fromList [(x, tv)]) tv, constrs = Sequence.empty}
--         OpVar _ mHash _ _ -> do
--           meta <- lookupEnv exprLoc (maybe (error "internal error, op vars must always be pinned!!") Left $ pinnedToMaybe mHash)
--           let (tcs, t) = ty meta
--           attachTypeToPosition exprLoc meta
--           pure InferResult{expr = expr, typ = t, constrs = foldMap (Sequence.singleton . Right . (exprLoc,)) tcs}
--         TypeRep _pos t -> pure InferResult{expr = expr, typ = ImplType mempty $ TRep t, constrs = Sequence.empty}
--         Enum _ mHash _ _ -> do
--           meta <- lookupEnv exprLoc (maybe (error "internal error, enums must always be pinned!!") Left $ pinnedToMaybe mHash)
--           let (_, t) = ty meta
--           attachTypeToPosition exprLoc meta{identExpr = bimap (const ()) (const ()) expr}
--           pure InferResult{expr = expr, typ = t, constrs = Sequence.empty}
--         InterpolatedString p1 xs p2 -> do
--           attachTypeToPosition
--             exprLoc
--             TypeMetadata
--               { identExpr = bimap (const ()) (const ()) $ removeComments expr
--               , ty = (Set.empty, ImplType Map.empty typeText)
--               , docs = Nothing
--               }
--           (xs', is, css) <-
--             unzip3
--               <$> forM
--                 (toEitherList xs)
--                 ( \case
--                     Left str -> pure (Left str, Map.empty, Sequence.empty)
--                     Right (p3, e, p4) ->
--                       (\r -> (Right (p3, r.expr, p4), r.typ.impl, r.constrs))
--                         <$> infer e
--                 )
--           let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) is
--           pure
--             InferResult
--               { expr = InterpolatedString p1 (fromEitherList xs') p2
--               , typ = ImplType isMerged typeText
--               , constrs = mconcat css <> Sequence.fromList ics
--               }
--         Record p1 fes p2 -> do
--           checkDuplicateFields exprLoc fes
--           let (fs, es) = unzip $ map (\(f, e, p) -> (f, (e, p))) fes
--           (es', impls, tys, cs) <- go es
--           let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) impls
--           let inferredTy = ImplType isMerged $ TRecord (Map.fromList $ zip fs tys) RowAbsent
--           let fes' = zipWith (\f (e, p) -> (f, e, p)) fs es'

--           attachTypeToPosition
--             exprLoc
--             TypeMetadata
--               { identExpr = bimap (const ()) (const ()) $ removeComments expr
--               , ty = (Set.empty, inferredTy)
--               , docs = Nothing
--               }
--           pure
--             InferResult
--               { expr = Record p1 fes' p2
--               , typ = inferredTy
--               , constrs = Sequence.fromList ics <> cs
--               }
--           where
--             go [] = pure ([], [], [], Sequence.empty)
--             go ((e', p3) : es') = do
--               r <- infer e'
--               (es'', impls, tRest, csRest) <- go es'
--               pure ((r.expr, p3) : es'', r.typ.impl : impls, r.typ.body : tRest, r.constrs <> csRest)
--         RecordField p_r (Ident rc) (Ident f) -> do
--           r <- infer $ Var p_r Local LocalScope $ Expl $ ExtIdent $ Right rc
--           tv <- fresh
--           trv <-
--             fresh >>= \case
--               TVar x -> pure x
--               _ -> error "fresh returned something other than a TVar"
--           let tyCls = Set.fromList $ map snd $ rights $ toList r.constrs
--           let tyRec = TRecord (Map.singleton (Ident f) tv) (RowVar trv)
--           pure
--             InferResult
--               { expr = expr
--               , typ = ImplType r.typ.impl tv
--               , constrs =
--                   r.constrs
--                     <> Sequence.fromList
--                       [ tyConstr r.typ.body tyRec [UnificationFail tyCls r.typ.body tyRec $ blockPosition expr]
--                       ]
--               }
--         Array _ [] _ -> do
--           tv <- fresh
--           let meta =
--                 TypeMetadata
--                   { identExpr = bimap (const ()) (const ()) $ removeComments expr
--                   , ty = (Set.empty, ImplType Map.empty $ TArray tv)
--                   , docs = Nothing
--                   }
--           let (_, t) = ty meta
--           attachTypeToPosition exprLoc meta
--           pure InferResult{expr = expr, typ = t, constrs = Sequence.empty}
--         Array p1 ((e, p2) : es) p3 -> do
--           r <- infer e
--           (es', impls, cs') <- go r.typ.body es
--           let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) $ r.typ.impl : impls
--           let inferredTy = ImplType isMerged $ TArray r.typ.body
--           attachTypeToPosition
--             exprLoc
--             TypeMetadata
--               { identExpr = bimap (const ()) (const ()) $ removeComments expr
--               , ty = (Set.empty, inferredTy)
--               , docs = Nothing
--               }

--           pure
--             InferResult
--               { expr = Array p1 ((r.expr, p2) : es') p3
--               , typ = inferredTy
--               , constrs = mconcat [Sequence.fromList ics, r.constrs, cs']
--               }
--           where
--             go _t [] = pure ([], [], Sequence.empty)
--             go t ((e', p4) : es') = do
--               r <- infer e'
--               (es'', impls, csRest) <- go t es'
--               pure
--                 ( (r.expr, p4) : es''
--                 , r.typ.impl : impls
--                 , mconcat
--                     [ r.constrs
--                     , csRest
--                     , Sequence.fromList
--                         [ tyConstr
--                             t
--                             r.typ.body
--                             [ UnificationFail
--                                 (Set.fromList . map snd . rights $ toList $ r.constrs <> csRest)
--                                 t
--                                 r.typ.body
--                                 $ blockPosition e'
--                             ]
--                         ]
--                     ]
--                 )
--         ArrayComp p1 e p2 sels cond p3 -> do
--           _ <- checkVariableOverlap $ NEList.toList sels
--           (sels', vars, is, css) <- unzip4 <$> go (NEList.toList sels) id

--           r1 <- foldr inEnv (infer e) vars

--           (cond', i_cond, c_cond) <- case cond of
--             Just (p4, e_cond) -> do
--               r2 <- foldr inEnv (infer e_cond) vars
--               pure
--                 ( Just (p4, r2.expr)
--                 , r2.typ.impl
--                 , r2.constrs
--                     <> Sequence.singleton (tyConstr r2.typ.body typeBool [UnificationFail (Set.fromList . map snd . rights $ toList r2.constrs) r2.typ.body typeBool $ blockPosition e_cond])
--                 )
--             Nothing -> pure (Nothing, Map.empty, Sequence.empty)

--           let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) $ [r1.typ.impl, i_cond] ++ is
--           pure
--             InferResult
--               { expr = ArrayComp p1 r1.expr p2 (NEList.fromList sels') cond' p3
--               , typ = ImplType isMerged (TArray r1.typ.body)
--               , constrs =
--                   mconcat
--                     [ Sequence.fromList ics
--                     , r1.constrs
--                     , c_cond
--                     , mconcat css
--                     ]
--               }
--           where
--             go [] _ = pure []
--             go ((pos, Ident x, p4, e_s, p5) : xs) f = do
--               r <- f $ infer e_s
--               tv <- fresh
--               attachTypeToPosition
--                 (elementPosition pos $ Ident x)
--                 TypeMetadata
--                   { identExpr = Var () () LocalScope $ Expl $ ExtIdent $ Right x
--                   , ty = (Set.fromList . map snd . rights $ toList r.constrs, ImplType r.typ.impl tv)
--                   , docs = Nothing
--                   }
--               let newEnv =
--                     ( ExtIdent $ Right x
--                     , TypeMetadata
--                         { identExpr = Var () () LocalScope $ Expl $ ExtIdent $ Right x
--                         , ty = ForallTC [] (Set.fromList . map snd . rights $ toList r.constrs) $ ImplType r.typ.impl tv
--                         , docs = Nothing
--                         }
--                     )
--               rest <- go xs (inEnv newEnv . f)
--               pure $
--                 ( (pos, Ident x, p4, r.expr, p5)
--                 , newEnv
--                 , r.typ.impl
--                 , r.constrs <> Sequence.singleton (tyConstr r.typ.body (TArray tv) [UnificationFail (Set.fromList . map snd . rights $ toList r.constrs) r.typ.body (TArray tv) $ blockPosition e_s])
--                 )
--                   : rest

--             checkVariableOverlap :: [(SourcePos, Ident, SourcePos, b, Maybe SourcePos)] -> Infer ()
--             checkVariableOverlap = \case
--               [] -> return ()
--               (loc, x, _, _e, _) : xs -> case find (\(_, x', _, _, _) -> x == x') xs of
--                 Just (loc', x', _, _, _) -> throwError [VarMultipleOccurrence x (elementPosition loc x) (elementPosition loc' x')]
--                 Nothing -> checkVariableOverlap xs
--         Lam p1 args p2 e -> do
--           r <- go $ NEList.toList args
--           pure InferResult{expr = Lam p1 args p2 r.expr, typ = r.typ, constrs = r.constrs}
--           where
--             go = \case
--               [] -> infer e
--               (pos, Just x) : xs -> do
--                 tv <- fresh
--                 let newEnv =
--                       ( x
--                       , TypeMetadata
--                           { identExpr = Var () () LocalScope $ Expl x
--                           , ty = ForallTC [] Set.empty $ ImplType Map.empty tv
--                           , docs = Nothing
--                           }
--                       )
--                 r <- inEnv newEnv $ go xs
--                 case x of
--                   ExtIdent (Left _) -> pure ()
--                   ExtIdent (Right i) ->
--                     attachTypeToPosition
--                       (elementPosition pos $ Just $ Ident i)
--                       TypeMetadata
--                         { identExpr = Var () () LocalScope $ Expl x
--                         , ty = (Set.empty, ImplType Map.empty tv)
--                         , docs = Nothing
--                         }
--                 pure InferResult{expr = r.expr, typ = ImplType r.typ.impl $ tv `TArr` r.typ.body, constrs = r.constrs}
--               (pos, Nothing) : xs -> do
--                 tv <- fresh
--                 attachTypeToPosition
--                   (elementPosition pos (Nothing :: Maybe Ident))
--                   TypeMetadata
--                     { identExpr = Var () () LocalScope $ Expl $ ExtIdent $ Right "_"
--                     , ty = (Set.empty, ImplType Map.empty tv)
--                     , docs = Nothing
--                     }
--                 r <- go xs
--                 pure InferResult{expr = r.expr, typ = ImplType r.typ.impl $ tv `TArr` r.typ.body, constrs = r.constrs}
--         App e1 e2 -> do
--           r1 <- infer e1
--           r2 <- infer e2

--           case r1.typ.body of
--             t1a `TArr` t1b -> do
--               tv <- fresh
--               let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) [r1.typ.impl, r2.typ.impl]
--                   tyCls = Set.fromList $ map snd $ rights $ toList $ r1.constrs <> r2.constrs
--               pure
--                 InferResult
--                   { expr = App r1.expr r2.expr
--                   , typ = ImplType isMerged tv
--                   , constrs =
--                       mconcat
--                         [ Sequence.fromList ics
--                         , r1.constrs
--                         , r2.constrs
--                         , Sequence.fromList
--                             [ tyConstr t1a r2.typ.body [UnificationFail tyCls t1a r2.typ.body $ blockPosition e2]
--                             , tyConstr t1b tv [UnificationFail tyCls t1b tv $ blockPosition expr]
--                             ]
--                         ]
--                   }
--             _ -> do
--               tv <- fresh
--               let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) [r1.typ.impl, r2.typ.impl]
--                   tyCls = Set.fromList $ map snd $ rights $ toList $ r1.constrs <> r2.constrs
--               -- if we end up on this branch, we will be throwing a unification error and
--               -- want to highlight e1, thus we attach `blockPosition e1` to the error
--               pure
--                 InferResult
--                   { expr = App r1.expr r2.expr
--                   , typ = ImplType isMerged tv
--                   , constrs =
--                       mconcat
--                         [ Sequence.fromList ics
--                         , r1.constrs
--                         , r2.constrs
--                         , Sequence.fromList
--                             [ tyConstr r1.typ.body (r2.typ.body `TArr` tv) [ExpectedFunction tyCls (r2.typ.body `TArr` tv) r1.typ.body $ blockPosition e1]
--                             ]
--                         ]
--                   }
--         LetAnnot p1 loc x pT t p2 e1 p3 e2 -> do
--           r1 <- infer e1
--           (tcs, ImplType iT tT) <- instantiate t
--           let tyCls = Set.fromList $ map snd $ rights $ toList r1.constrs
--           attachTypeToPosition
--             (elementPosition loc $ Expl x)
--             TypeMetadata
--               { identExpr = Var () () LocalScope $ Expl x
--               , -- ty = (tyCls, ImplType r1.typ.impl r1.typ.body),
--                 ty = (tcs, ImplType iT tT)
--               , docs = Nothing
--               }
--           let newEnv =
--                 ( x
--                 , TypeMetadata
--                     { identExpr = Var () () LocalScope $ Expl x
--                     , -- TODO should this instead be: ty = t,
--                       ty = ForallTC [] tcs (ImplType iT tT)
--                     , docs = Nothing
--                     }
--                 )
--           r2 <- inEnv newEnv $ infer e2
--           let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) [r1.typ.impl, r2.typ.impl, iT]
--           pure
--             InferResult
--               { expr = LetAnnot p1 loc x pT t p2 r1.expr p3 r2.expr
--               , typ = ImplType isMerged r2.typ.body
--               , constrs =
--                   mconcat
--                     [ Sequence.fromList ics
--                     , r1.constrs
--                     , r2.constrs
--                     , -- Type of e1 == type annotation
--                       Sequence.fromList [tyConstr r1.typ.body tT [AnnotationUnificationFail tyCls r1.typ.body tT $ blockPosition e1]]
--                     , -- Type class constraints from type annotation TODO filter out reps?
--                       foldMap (Sequence.singleton . Right . (exprLoc,)) tcs
--                     ]
--               }
--         -- non generalized let
--         Let p1 loc (Expl x) p2 e1 p3 e2 -> do
--           r1 <- infer e1
--           let tyCls = Set.fromList $ map snd $ rights $ toList r1.constrs
--           attachTypeToPosition
--             (elementPosition loc $ Expl x)
--             TypeMetadata
--               { identExpr = Var () () LocalScope $ Expl x
--               , ty = (tyCls, ImplType r1.typ.impl r1.typ.body)
--               , docs = Nothing
--               }

--           let newEnv =
--                 ( x
--                 , TypeMetadata
--                     { identExpr = Var () () LocalScope $ Expl x
--                     , ty = ForallTC [] tyCls $ ImplType r1.typ.impl r1.typ.body
--                     , docs = Nothing
--                     }
--                 )
--           r2 <- inEnv newEnv $ infer e2
--           let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) [r1.typ.impl, r2.typ.impl]
--           pure
--             InferResult
--               { expr = Let p1 loc (Expl x) p2 r1.expr p3 r2.expr
--               , typ = ImplType isMerged r2.typ.body
--               , constrs = mconcat [Sequence.fromList ics, r1.constrs, r2.constrs]
--               }
--         Let p1 loc (Impl x) p2 e1 p3 e2 -> do
--           r1 <- infer e1
--           r2 <- infer e2

--           v1 <- maybe fresh pure (Map.lookup x r2.typ.impl)

--           let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) [r1.typ.impl, Map.withoutKeys r2.typ.impl (Set.singleton x)]
--               tyCls = Set.fromList $ map snd $ rights $ toList $ r1.constrs <> r2.constrs

--           pure
--             InferResult
--               { expr = Let p1 loc (Impl x) p2 r1.expr p3 r2.expr
--               , typ = ImplType isMerged r2.typ.body
--               , constrs =
--                   mconcat
--                     [ Sequence.fromList ics
--                     , r1.constrs
--                     , r2.constrs
--                     , Sequence.singleton (tyConstr v1 r1.typ.body [ImplicitVarTypeOverlap tyCls x v1 r1.typ.body $ blockPosition expr])
--                     ]
--               }
--         Op e1 loc mHash opMeta modNm op e2 -> do
--           let (sPos, ePos) = elementPosition loc op
--           let opLoc = (sPos, incSourceCol ePos $ fromScoped 0 $ (+ 1) . Text.length . unModuleName <$> modNm)

--           r1 <- infer e1
--           r2 <- infer e2

--           meta <- lookupEnv opLoc (maybe (error "internal error, infix ops must always be pinned!!") Left $ pinnedToMaybe mHash)
--           let (tcs, (u1, u2, u3)) = opGetTyComponents <$> ty meta

--           tv <- fresh
--           let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) [r1.typ.impl, r2.typ.impl]
--               tyCls = Set.fromList $ map snd $ rights $ toList $ r1.constrs <> r2.constrs

--           attachTypeToPosition opLoc meta{ty = (tcs, ImplType Map.empty $ r1.typ.body `TArr` (r2.typ.body `TArr` tv))}

--           pure
--             InferResult
--               { expr = Op r1.expr loc mHash opMeta modNm op r2.expr
--               , typ = ImplType isMerged tv
--               , constrs =
--                   mconcat
--                     [ Sequence.fromList ics
--                     , r1.constrs
--                     , r2.constrs
--                     , Sequence.fromList
--                         [ tyConstr u1 r1.typ.body [UnificationFail tyCls u1 r1.typ.body $ blockPosition e1]
--                         , tyConstr u2 r2.typ.body [UnificationFail tyCls u2 r2.typ.body $ blockPosition e2]
--                         , tyConstr u3 tv [UnificationFail tyCls u3 tv $ blockPosition expr]
--                         ]
--                     , foldMap (Sequence.singleton . Right . (opLoc,)) tcs
--                     ]
--               }
--         PreOp loc mHash opMeta modNm op e -> do
--           let (sPos, ePos) = elementPosition loc op
--           let opLoc = (sPos, incSourceCol ePos $ fromScoped 0 $ (+ 1) . Text.length . unModuleName <$> modNm)

--           r <- infer e

--           meta <- lookupEnv opLoc (maybe (error "internal error, prefix ops must always be pinned!!") Left $ pinnedToMaybe mHash)
--           let (tcs, (u1, u2)) = preOpGetTyComponents <$> ty meta
--               tyCls = Set.fromList $ map snd $ rights $ toList r.constrs

--           tv <- fresh
--           attachTypeToPosition opLoc meta{ty = (tcs, ImplType Map.empty $ r.typ.body `TArr` tv)}

--           pure
--             InferResult
--               { expr = PreOp loc mHash opMeta modNm op r.expr
--               , typ = ImplType r.typ.impl tv
--               , constrs =
--                   mconcat
--                     [ r.constrs
--                     , Sequence.fromList
--                         [ tyConstr u1 r.typ.body [UnificationFail tyCls u1 r.typ.body $ blockPosition e]
--                         , tyConstr u2 tv [UnificationFail tyCls u2 tv $ blockPosition expr]
--                         ]
--                     , foldMap (Sequence.singleton . Right . (opLoc,)) tcs
--                     ]
--               }
--         If p1 cond p2 tr p3 fl -> do
--           r1 <- infer cond
--           r2 <- infer tr
--           r3 <- infer fl

--           let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) [r1.typ.impl, r2.typ.impl, r3.typ.impl]
--               tyCls = Set.fromList $ map snd $ rights $ toList $ r1.constrs <> r2.constrs <> r3.constrs

--           pure
--             InferResult
--               { expr = If p1 r1.expr p2 r2.expr p3 r3.expr
--               , typ = ImplType isMerged r2.typ.body
--               , constrs =
--                   mconcat
--                     [ Sequence.fromList ics
--                     , r1.constrs
--                     , r2.constrs
--                     , r3.constrs
--                     , Sequence.fromList
--                         [ tyConstr r1.typ.body typeBool [IfConditionMustBeBool tyCls r1.typ.body $ blockPosition cond]
--                         , tyConstr r2.typ.body r3.typ.body [IfBranchesMustBeEqType tyCls r2.typ.body r3.typ.body (blockPosition tr) (blockPosition fl)]
--                         ]
--                     ]
--               }
--         Tuple p1 es p2 -> do
--           (es', impls, tys, cs) <- go $ tListToList es
--           let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) impls
--           let inferredTy = ImplType isMerged $ TTuple $ tListFromList tys
--           attachTypeToPosition
--             exprLoc
--             TypeMetadata
--               { identExpr = bimap (const ()) (const ()) $ removeComments expr
--               , ty = (Set.empty, inferredTy)
--               , docs = Nothing
--               }

--           pure
--             InferResult
--               { expr = Tuple p1 (tListFromList es') p2
--               , typ = inferredTy
--               , constrs = Sequence.fromList ics <> cs
--               }
--           where
--             go [] = pure ([], [], [], Sequence.empty)
--             go ((e', p3) : es') = do
--               r <- infer e'
--               (es'', impls, tRest, csRest) <- go es'
--               pure ((r.expr, p3) : es'', r.typ.impl : impls, r.typ.body : tRest, r.constrs <> csRest)
--         Assert p1 cond p2 e -> do
--           r1 <- infer cond
--           r2 <- infer e

--           let (isMerged, ics) = mergeImplicitMaps (blockPosition expr) [r1.typ.impl, r2.typ.impl]
--           pure
--             InferResult
--               { expr = Assert p1 r1.expr p2 r2.expr
--               , typ = ImplType isMerged r2.typ.body
--               , constrs =
--                   mconcat
--                     [ Sequence.fromList ics
--                     , r1.constrs
--                     , r2.constrs
--                     , Sequence.singleton $
--                         tyConstr
--                           r1.typ.body
--                           typeBool
--                           [ AssertConditionMustBeBool
--                               (Set.fromList . fmap snd . rights $ toList $ r1.constrs <> r2.constrs)
--                               r1.typ.body
--                               $ blockPosition cond
--                           ]
--                     ]
--               }
--         Empty _ -> do
--           meta <- lookupEnv exprLoc $ Left emptyHash
--           let (_, t) = ty meta
--           attachTypeToPosition exprLoc meta
--           pure InferResult{expr = expr, typ = t, constrs = Sequence.empty}
--         One p e -> do
--           r <- infer e
--           meta <- lookupEnv exprLoc $ Left oneHash
--           attachTypeToPosition exprLoc meta{ty = (Set.empty, ImplType r.typ.impl $ TOptional r.typ.body)}
--           pure InferResult{expr = One p r.expr, typ = ImplType r.typ.impl $ TOptional r.typ.body, constrs = r.constrs}
--         Case p1 e p2 patExprs' p3 -> do
--           let branches = [CaseBranch bp pt ap bd | (bp, pt, ap, bd) <- NEList.toList patExprs']
--               pats = fmap (.pat) branches
--           r <- infer e
--           (patTys, patVars, patConstraints) <-
--             unzip3
--               <$> mapM (\p -> checkVariableOverlap Map.empty p >> mkPatConstraint p) pats

--           addCasePatterns exprLoc pats

--           res <- forM (zip patVars $ fmap (.body) branches) $
--             \(vars, e''') -> foldr (inEnv . (\(Ident x, meta) -> (ExtIdent $ Right x, meta))) (infer e''') vars

--           let es'' = fmap (.expr) res
--               is_res = fmap (\ri -> ri.typ.impl) res
--               ts_res = fmap (\ri -> ri.typ.body) res
--               cs_res = fmap (.constrs) res
--               (isMerged, ics) = mergeImplicitMaps (blockPosition expr) (r.typ.impl : is_res)
--               tyCls = Set.fromList $ map snd $ rights $ toList $ r.constrs <> mconcat cs_res
--               patTysEqConstraints =
--                 Sequence.fromList
--                   [ tyConstr tPat4 tPat5 [PatternsMustBeEqType tyCls tPat4 tPat5 p4 p5 (blockPosition p4) (blockPosition p5)]
--                   | (tPat4, p4) <- zip patTys pats
--                   , (tPat5, p5) <- zip patTys pats
--                   , p4 /= p5
--                   ]
--               patTysMustEqCaseExprTy cExprTy =
--                 Sequence.fromList
--                   [ tyConstr tPat cExprTy [PatternUnificationFail tPat cExprTy p $ blockPosition p]
--                   | (tPat, p) <- zip patTys pats
--                   ]
--               patExpTysEqConstraints set =
--                 Sequence.fromList
--                   [ tyConstr t1 t2 [CaseBranchesMustBeEqType tyCls t1 t2 (blockPosition e1) (blockPosition e2)]
--                   | (ImplType _ t1, e1) <- set
--                   , (ImplType _ t2, e2) <- set
--                   , e1 /= e2
--                   ]

--           case ts_res of
--             (tRes : _) ->
--               pure
--                 InferResult
--                   { expr = Case p1 r.expr p2 (NEList.fromList $ zipWith (\e'' b -> (b.barPos, b.pat, b.arrPos, e'')) es'' branches) p3
--                   , typ = ImplType isMerged tRes
--                   , constrs =
--                       mconcat
--                         [ Sequence.fromList ics
--                         , r.constrs
--                         , mconcat patConstraints
--                         , patTysEqConstraints
--                         , patTysMustEqCaseExprTy r.typ.body
--                         , patExpTysEqConstraints (zip (fmap (.typ) res) (fmap (.body) branches))
--                         , mconcat cs_res
--                         ]
--                   }
--             -- `patExprs'` is `NonEmpty`, so `ts_res` is always non-empty
--             [] -> error "impossible: case expression must have at least one branch"
--           where
--             mkPatConstraint :: Pat (Pinned VCObjectHash) SourcePos -> Infer (InfernoType, [(Ident, TypeMetadata TCScheme)], Seq Constraint)
--             mkPatConstraint pat =
--               let patLoc = blockPosition pat
--                in case pat of
--                     PVar _ (Just (Ident x)) -> do
--                       tv <- fresh
--                       attachTypeToPosition
--                         patLoc
--                         TypeMetadata
--                           { identExpr = Var () () LocalScope $ Expl $ ExtIdent $ Right x
--                           , ty = (Set.empty, ImplType Map.empty tv)
--                           , docs = Nothing
--                           }
--                       let meta =
--                             TypeMetadata
--                               { identExpr = Var () () LocalScope $ Expl $ ExtIdent $ Right x
--                               , ty = ForallTC [] Set.empty $ ImplType Map.empty tv
--                               , docs = Nothing
--                               }
--                       pure (tv, [(Ident x, meta)], Sequence.empty)
--                     PEnum _ Local _ _ -> error "internal error, malformed pattern enum must be pinned"
--                     PEnum _ hash sc i -> do
--                       meta <- lookupEnv patLoc $ Left $ fromJust $ pinnedToMaybe hash
--                       let (_, ImplType _ t) = ty meta
--                       attachTypeToPosition patLoc meta{identExpr = Enum () () sc i}
--                       pure (t, [], Sequence.empty)
--                     PLit _ l ->
--                       inferPatLit
--                         patLoc
--                         l
--                         ( case l of
--                             LInt _ -> typeInt
--                             LDouble _ -> typeDouble
--                             LHex _ -> typeWord64
--                             LText _ -> typeText
--                         )
--                     POne _ p -> do
--                       (t, vars, cs) <- mkPatConstraint p
--                       meta <- lookupEnv patLoc $ Left oneHash
--                       attachTypeToPosition patLoc meta{ty = (Set.empty, ImplType Map.empty $ t .-> TOptional t)}
--                       pure (TOptional t, vars, cs)
--                     PEmpty _ -> do
--                       meta <- lookupEnv patLoc $ Left emptyHash
--                       let (_, ImplType _ t) = ty meta
--                       attachTypeToPosition patLoc meta
--                       pure (t, [], Sequence.empty)
--                     PArray _ [] _ -> do
--                       tv <- fresh
--                       let t = TArray tv
--                       attachTypeToPosition
--                         patLoc
--                         TypeMetadata
--                           { identExpr = patternToExpr $ bimap (const ()) (const ()) pat
--                           , ty = (Set.empty, ImplType Map.empty t)
--                           , docs = Nothing
--                           }
--                       pure (t, [], Sequence.empty)
--                     PArray _ ((p, _) : ps) _ -> do
--                       (t, vars1, csP) <- mkPatConstraint p
--                       (vars2, csPs) <- aux t ps
--                       let inferredTy = TArray t
--                       attachTypeToPosition
--                         patLoc
--                         TypeMetadata
--                           { identExpr = patternToExpr $ bimap (const ()) (const ()) pat
--                           , ty = (Set.empty, ImplType Map.empty inferredTy)
--                           , docs = Nothing
--                           }
--                       pure (inferredTy, vars1 ++ vars2, csP <> csPs)
--                       where
--                         aux _t [] = pure ([], Sequence.empty)
--                         aux t ((p', _) : ps') = do
--                           (t', vars', cs') <- mkPatConstraint p'
--                           (vars, cs) <- aux t ps'
--                           let tIst' = tyConstr t t' [UnificationFail Set.empty t t' $ blockPosition p']
--                           pure
--                             ( vars' ++ vars
--                             , mconcat [cs', cs, Sequence.singleton tIst']
--                             )
--                     PTuple _ ps _ -> do
--                       (ts, vars, cs) <- aux $ tListToList ps
--                       let inferredTy = TTuple $ tListFromList ts
--                       attachTypeToPosition
--                         patLoc
--                         TypeMetadata
--                           { identExpr = patternToExpr $ bimap (const ()) (const ()) pat
--                           , ty = (Set.empty, ImplType Map.empty inferredTy)
--                           , docs = Nothing
--                           }
--                       pure (inferredTy, vars, cs)
--                       where
--                         aux [] = pure ([], [], Sequence.empty)
--                         aux ((p', _l) : ps') = do
--                           (t, vars1, cs1) <- mkPatConstraint p'
--                           (ts, vars2, cs2) <- aux ps'
--                           pure (t : ts, vars1 ++ vars2, cs1 <> cs2)
--                     PRecord _ fs _ -> do
--                       checkDuplicateFields patLoc fs
--                       (ts, vars, cs) <- aux fs
--                       let inferredTy = TRecord ts RowAbsent
--                       attachTypeToPosition
--                         patLoc
--                         TypeMetadata
--                           { identExpr = patternToExpr $ bimap (const ()) (const ()) pat
--                           , ty = (Set.empty, ImplType Map.empty inferredTy)
--                           , docs = Nothing
--                           }
--                       pure (inferredTy, vars, cs)
--                       where
--                         aux [] = pure (mempty, [], Sequence.empty)
--                         aux ((f, p', _l) : ps') = do
--                           (t, vars1, cs1) <- mkPatConstraint p'
--                           (ts, vars2, cs2) <- aux ps'
--                           pure (Map.insert f t ts, vars1 ++ vars2, cs1 <> cs2)
--                     PVar _ Nothing -> do
--                       tv <- fresh
--                       let meta =
--                             TypeMetadata
--                               { identExpr = patternToExpr $ bimap (const ()) (const ()) pat
--                               , ty = (Set.empty, ImplType Map.empty tv)
--                               , docs = Nothing
--                               }
--                       attachTypeToPosition patLoc meta
--                       pure (tv, [], Sequence.empty)
--                     PCommentAbove _ p -> mkPatConstraint p
--                     PCommentAfter p _ -> mkPatConstraint p
--                     PCommentBelow p _ -> mkPatConstraint p

--             checkVariableOverlap :: Map.Map Ident (Location SourcePos) -> Pat (Pinned VCObjectHash) SourcePos -> Infer (Map.Map Ident (Location SourcePos))
--             checkVariableOverlap vars pat =
--               let patLoc = blockPosition pat
--                in case pat of
--                     PVar _ (Just x) -> case Map.lookup x vars of
--                       Just loc' -> throwError [VarMultipleOccurrence x patLoc loc']
--                       Nothing -> return $ Map.insert x patLoc vars
--                     POne _ p -> checkVariableOverlap vars p
--                     PArray _ ps _ -> foldM checkVariableOverlap vars $ map fst ps
--                     PTuple _ ps _ -> foldM checkVariableOverlap vars $ map fst $ tListToList ps
--                     PRecord _ ps _ -> foldM checkVariableOverlap vars $ map snd3 ps
--                     _ -> return vars
--         CommentAbove p e -> do
--           r <- infer e
--           pure InferResult{expr = CommentAbove p r.expr, typ = r.typ, constrs = r.constrs}
--         CommentAfter e p -> do
--           r <- infer e
--           pure InferResult{expr = CommentAfter r.expr p, typ = r.typ, constrs = r.constrs}
--         CommentBelow e p -> do
--           r <- infer e
--           pure InferResult{expr = CommentBelow r.expr p, typ = r.typ, constrs = r.constrs}
--         Bracketed p1 e p2 -> do
--           r <- infer e
--           pure InferResult{expr = Bracketed p1 r.expr p2, typ = r.typ, constrs = r.constrs}
--         RenameModule l1 newNm l2 oldNm l3 e -> do
--           s@InferState{modules = mods} <- get
--           when (newNm `Map.member` mods) $ throwError [ModuleNameTaken newNm $ elementPosition l1 newNm]
--           case Map.lookup oldNm mods of
--             Nothing -> throwError [ModuleDoesNotExist oldNm (l2, l3)]
--             Just oldNmMod -> do
--               put s{modules = Map.insert newNm oldNmMod mods}
--               r <- infer e
--               modify $ \s' -> s'{modules = Map.delete newNm s.modules}
--               pure InferResult{expr = RenameModule l1 newNm l2 oldNm l3 r.expr, typ = r.typ, constrs = r.constrs}
--         OpenModule l1 mHash modNm@(ModuleName n) imports p e -> do
--           InferState{modules = mods} <- get
--           case Map.lookup modNm mods of
--             Nothing -> throwError [ModuleDoesNotExist modNm $ elementPosition l1 $ Ident n]
--             Just _openMod -> do
--               r <- infer e
--               pure InferResult{expr = OpenModule l1 mHash modNm imports p r.expr, typ = r.typ, constrs = r.constrs}
--   where
--     -- Check if a record expr/pat has a duplicate field name
--     checkDuplicateFields l = aux mempty
--       where
--         aux _seen [] = pure ()
--         aux seen ((f, _, _) : fs')
--           | Set.member f seen = throwError [DuplicateRecordField f l]
--           | otherwise = aux (Set.insert f seen) fs'

-- inferPatLit :: Location SourcePos -> Lit -> InfernoType -> Infer (InfernoType, [b], Seq c)
-- inferPatLit loc n t =
--   attachTypeToPosition
--     loc
--     TypeMetadata
--       { identExpr = Lit () n
--       , ty = (Set.empty, ImplType Map.empty t)
--       , docs = Nothing
--       }
--     >> pure (t, [], Sequence.empty)

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

-- -- | The empty substitution
-- emptySubst :: Subst
-- emptySubst = mempty

-- -- | Compose substitutions
-- compose :: Subst -> Subst -> Subst
-- (Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

-- -- | Run the constraint solver
-- runSolve :: Int -> Set.Set TypeClass -> [Constraint] -> Either [TypeError SourcePos] Subst
-- runSolve varCount allClasses cs = runIdentity $ runExceptT $ flip runReaderT allClasses $ solver varCount st
--   where
--     st = (emptySubst, cs)

-- unifyMany :: [TypeError SourcePos] -> [InfernoType] -> [InfernoType] -> SolveState Int Subst
-- unifyMany _ [] [] = return emptySubst
-- unifyMany err (t1 : ts1) (t2 : ts2) = do
--   su1 <- unifies err t1 t2
--   su2 <- unifyMany err (apply su1 ts1) (apply su1 ts2)
--   return (su2 `compose` su1)
-- unifyMany err _ _ = trace "throwing in unifyMany " $ throwError err

-- | Unify the fields of two record types.
--
-- The idea in theory is, if we are trying to unify
-- @
--  {f1: tf1, ..., g1: tg1, ..., 'a}
--              ~ {g1: tg1', ..., f1': tf1', ..., 'b}
-- @
-- where g1, ... gN are the common fields, then we first expand the row vars 'a and 'b
-- so that we have the same set of field names on both sides:
-- @
--   {f1: tf1, ..., g1: tg1, ..., f1': 't1', ..., 'c}
-- ~ {f1: 't1, ..., g1: tg1', ..., f1': tf1', ..., 'd}
-- @
-- where 't1, 't2, ... and 't1', 't2', ... and 'c and 'd are fresh type vars.
-- Then, we match up the field names and unify the field types.
-- Finally, we add the substs:
-- @
--  'a ~> {f1': 't1', ..., 'c}
--  'b ~> {f1: 't1, ..., 'd}
-- @
--
-- The implementation in practice proceeds by recursing on the fields of both records.
-- We first convert the Map of field types to a *sorted* association list, and recurse
-- on both association lists in the style of mergesort's merge. When the two smallest
-- field names are the same, we unify the field types. Otherwise, we take the smallest
-- and create a fresh type var and add it as a new field to the other side.
-- This function recurses with arguments that include a list of new fields for each
-- record (in *descending* order of field names), and the list of pairs of field types
-- that must be unified.
--
-- Reference:
-- https://gallium.inria.fr/~remy/ftp/habil.pdf (Chapter 2)
-- https://gallium.inria.fr/~remy/ftp/record-algebras.pdf
-- (Though the algorithm implemented here is based on my understanding of how the
-- theoretical algorithms in the above papers would be implemented in practice.)
unifyRecords ::
  [TypeError SourcePos] ->
  ([(Ident, InfernoType)], RestOfRecord) ->
  ([(Ident, InfernoType)], RestOfRecord) ->
  [(Ident, InfernoType)] ->
  [(Ident, InfernoType)] ->
  [(InfernoType, InfernoType)] ->
  SolveState Int Subst
-- unifyRecords = err ([], trv1) ([], trv2) newFields1 newFields2 pairs
unifyRecords = undefined
--   -- Base case: when all fields are expanded and matched up:
--   -- trace ("End unifyRecords: " <> show newFields1 <> " " <> show newFields2) $ pure ()
--   -- If new fields were added, create a fresh row var and a substitution for it
--   (trv1', su1) <- makeRowVarSubst trv1 newFields1
--   (trv2', su2) <- makeRowVarSubst trv2 newFields2
--   let su = Subst $ Map.fromList $ su1 ++ su2
--   -- Unify all pairs of field types and the final row vars
--   let pairsToUnify = unifyRowVars trv1' trv2' ++ pairs
--   su' <- uncurry (unifyMany err) $ unzip pairsToUnify
--   -- Apply su' to su so that the fields and row vars in su are unified
--   pure $ su' `compose` su
--   where
--     unifyRowVars RowAbsent RowAbsent = []
--     unifyRowVars (RowVar tv) RowAbsent = [(TVar tv, TRecord mempty RowAbsent)]
--     unifyRowVars RowAbsent (RowVar tv) = [(TVar tv, TRecord mempty RowAbsent)]
--     unifyRowVars (RowVar tv) (RowVar tv') = [(TVar tv, TVar tv')]
--     -- Create a subst from a new row var to the new fields
--     makeRowVarSubst trv [] =
--       pure (trv, [])
--     makeRowVarSubst RowAbsent _ =
--       error "impossible: newFields created when RowVar is RowAbsent"
--     makeRowVarSubst (RowVar tv) newFields = do
--       tv' <- freshTypeVar
--       pure (RowVar tv', [(tv, TRecord (Map.fromDescList newFields) (RowVar tv'))])
-- unifyRecords err ([], trv1) ((f2, t2) : ts2, trv2) newFields1 newFields2 pairs = do
--   tv <- expandRowVar err trv1
--   let pairs' = (tv, t2) : pairs
--   let newFields1' = (f2, tv) : newFields1
--   unifyRecords err ([], trv1) (ts2, trv2) newFields1' newFields2 pairs'
-- unifyRecords err ((f1, t1) : ts1, trv1) ([], trv2) newFields1 newFields2 pairs = do
--   tv <- expandRowVar err trv2
--   let pairs' = (tv, t1) : pairs
--   let newFields2' = (f1, tv) : newFields2
--   unifyRecords err (ts1, trv1) ([], trv2) newFields1 newFields2' pairs'
-- unifyRecords err ((f1, t1) : ts1, trv1) ((f2, t2) : ts2, trv2) newFields1 newFields2 pairs
--   | f1 == f2 = do
--       let pairs' = (t1, t2) : pairs
--       unifyRecords err (ts1, trv1) (ts2, trv2) newFields1 newFields2 pairs'
--   | f1 > f2 = do
--       tv <- expandRowVar err trv1
--       let pairs' = (tv, t2) : pairs
--       let newFields1' = (f2, tv) : newFields1
--       unifyRecords err ((f1, t1) : ts1, trv1) (ts2, trv2) newFields1' newFields2 pairs'
--   | otherwise = do
--       tv <- expandRowVar err trv2
--       let pairs' = (tv, t1) : pairs
--       let newFields2' = (f1, tv) : newFields2
--       unifyRecords err (ts1, trv1) ((f2, t2) : ts2, trv2) newFields1 newFields2' pairs'

-- -- | If the rest of the record is a row variable, this generates a fresh type var to
-- -- denote a new field. Otherwise, it throws a type error.
-- expandRowVar :: [TypeError SourcePos] -> RestOfRecord -> SolveState Int InfernoType
-- expandRowVar err RowAbsent = throwError err
-- expandRowVar _err (RowVar _) = TVar <$> freshTypeVar

-- freshTypeVar :: SolveState Int TV
-- freshTypeVar = do
--   count <- get
--   put $ count + 1
--   return $ TV count

-- unifies :: [TypeError SourcePos] -> InfernoType -> InfernoType -> SolveState Int Subst
-- -- unifies _ t1 t2 | trace (Text.unpack ("unifying " <> renderPretty t1 <> " and " <> renderPretty t2)) False = undefined
-- unifies _ t1 t2 | t1 == t2 = return emptySubst
-- unifies err (TVar v) t = bind err v t
-- unifies err t (TVar v) = bind err v t
-- unifies err (TArr t1 t2) (TArr t3 t4) = unifyMany err [t1, t2] [t3, t4]
-- unifies err (TArray t1) (TArray t2) = unifies err t1 t2
-- unifies err (TSeries t1) (TSeries t2) = unifies err t1 t2
-- unifies err (TOptional t1) (TOptional t2) = unifies err t1 t2
-- unifies err (TTuple ts1) (TTuple ts2)
--   | length (tListToList ts1) == length (tListToList ts2) = unifyMany err (tListToList ts1) (tListToList ts2)
--   | otherwise = throwError [UnificationFail (getTypeClassFromErrs err) (TTuple ts1) (TTuple ts2) loc | loc <- getLocFromErrs err]
-- unifies err (TRecord ts1 trv1) (TRecord ts2 trv2) =
--   unifyRecords err (Map.toAscList ts1, trv1) (Map.toAscList ts2, trv2) [] [] []
-- unifies err _ _ =
--   -- trace "throwing in unifies " $
--   throwError err

-- -- Unification solver
-- solver :: Int -> Unifier -> Solve Subst
-- solver varCount (su, cs) =
--   case cs of
--     [] -> return su
--     _ -> do
--       let (tyConstrs, typeCls) = partitionEithers cs
--       su1 <- flip evalSolveState varCount $ solverTyCs su tyConstrs
--       -- trace ("After solverTyCs, final su1\n" <> show su1) $ pure ()
--       let partResolvedTyCls = map (second (apply su1)) typeCls
--       -- trace ("partResolvedTyCls: " <> (intercalate "\n" $ map (unpack . renderPretty . pretty . snd) partResolvedTyCls)) $
--       evalSolveState (solverTypeClasses $ su1 `compose` su) (Set.fromList partResolvedTyCls, mempty)

-- solverTyCs :: Subst -> [(InfernoType, InfernoType, [TypeError SourcePos])] -> SolveState Int Subst
-- -- solverTyCs su _cs | trace ("solverTyCs with su: " <> show su) False = undefined
-- solverTyCs su cs =
--   case cs of
--     [] -> return su
--     ((t1, t2, errs) : cs0) -> do
--       su1 <- unifies errs t1 t2
--       -- trace ("In solverTyCs, applying su1\n" <> show su1 <> "\nOnto es\n" <> show cs0) $ pure ()
--       solverTyCs (su1 `compose` su) (map (\(t1', t2', es) -> (apply su1 t1', apply su1 t2', map (apply su1) es)) cs0)

-- evalSolveState :: SolveState st a -> st -> Solve a
-- evalSolveState (ReaderT f) st = ReaderT $ \r -> evalStateT (f r) st

-- liftToSolveState :: Solve a -> SolveState st a
-- liftToSolveState (ReaderT f) = ReaderT $ \r -> StateT $ \s -> (,s) <$> f r

-- pick :: (Show a, Ord a) => SolveState (Set.Set a, Set.Set a) (Maybe a)
-- pick = state $ \st@(current, marked) ->
--   case Set.lookupMin current of
--     Nothing -> (Nothing, st)
--     Just a -> (Just a, (Set.delete a current, Set.insert a marked))

-- -- | `applySubsts` applies the substitution `su` on both marked and unmarked typeclasses and an new information, propagated to
-- --   marked typeclasses, causes said typeclass to be moved back to the "current" set.
-- --   We then filter out any fully resolved classes in the marked set only!! to avoid extra unnecessary steps.
-- --   (Filtering the unprocessed, i.e. current classes may lead to subtle bugs if the class is fully instantiated but
-- --   is not in fact an instance found in `allClasses`)
-- applySubsts :: (Ord loc) => Subst -> SolveState (Set.Set (loc, TypeClass), Set.Set (loc, TypeClass)) ()
-- applySubsts su = state $ \(current, marked) ->
--   (\(c, m) -> ((), (c, filterFullyInstantiated m))) $
--     foldr
--       ( \(loc, a) (current', marked') ->
--           let a' = apply su a
--            in if a == a'
--                 then (current', Set.insert (loc, a') marked')
--                 else (Set.insert (loc, a') current', marked')
--       )
--       (Set.map (Data.Bifunctor.second (apply su)) current, mempty)
--       marked
--   where
--     filterFullyInstantiated =
--       Set.filter $
--         not
--           . Set.null
--           . ftv
--           . snd

-- solverTypeClasses :: Subst -> SolveState (Set.Set (Location SourcePos, TypeClass), Set.Set (Location SourcePos, TypeClass)) Subst
-- solverTypeClasses su =
--   pick >>= \case
--     Nothing -> return su
--     Just (loc, tc@(TypeClass nm tys)) -> do
--       allClasses <- ask
--       let matchingInstances = Set.toList $ Set.filter (\(TypeClass nm' _) -> nm == nm') allClasses
--       if null matchingInstances
--         then throwError [TypeClassNotFoundError allClasses tc loc]
--         else do
--           res <- liftToSolveState (catMaybes <$> forM matchingInstances (tryMatchPartial tys))
--           case res of
--             [] -> throwError [TypeClassNoPartialMatch tc loc]
--             (Subst s : xs) -> do
--               -- even if we have multiple matching substitutions, we can still make progress if they all agree
--               -- on some parameter
--               let su' = Subst (foldr intersection s [x | Subst x <- xs]) `compose` su
--               -- trace ("applying su': "<> show su' <> "\nprevious was su: " <> show su) $
--               applySubsts su'
--               solverTypeClasses su'
--   where
--     intersection = Map.merge Map.dropMissing Map.dropMissing (Map.zipWithMaybeMatched $ \_ a b -> if a == b then Just a else Nothing)

-- newtype Counter = Counter Int

-- -- | Use `getLit` if you want to remember what it points to, i.e. if mapping a (TVar, InfernoType) to a SAT solver variable
-- getLit :: (MonadState s m, HasType (Bimap.Bimap Int a) s, HasType Counter s, Ord a) => a -> m Int
-- getLit a = do
--   st <- get
--   let bm = getTyped st
--   let Counter i = getTyped st
--   case Bimap.lookupR a bm of
--     Just l -> pure l
--     Nothing -> do
--       put $ setTyped (Counter $ i + 1) $ setTyped (Bimap.insert i a bm) st
--       pure i

-- -- | Return a fresh SAT solver variable
-- newLit :: (MonadState s m, HasType Counter s) => m Int
-- newLit = do
--   st <- get
--   let Counter i = getTyped st
--   put $ setTyped (Counter $ i + 1) st
--   pure i

-- addClause :: (MonadState s m, HasType [[Int]] s) => [Int] -> m ()
-- addClause c = do
--   st <- get
--   let clauses = getTyped st
--   put $ setTyped (c : clauses) st

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
-- findTypeClassWitnesses allClasses iters tyCls tvs =
findTypeClassWitnesses = undefined
  -- unsafePerformIO $
  --   Picosat.evalScopedPicosat $
  --     Picosat.addBaseClauses clauses >> getSolutions iters
  -- where
  --   filteredSubs = filteredTypeClassSubstitutions allClasses $ Set.toList tyCls
  --   (_, litMap, clauses) = flip execState (Counter 1, Bimap.empty, []) $ do
  --     encodeTypeClasses allClasses filteredSubs $ Set.toList tyCls
  --     lm :: Bimap.Bimap Int (TV, InfernoType) <- gets getTyped
  --     let ls_grouped = foldr (\(l, (tv, _)) m' -> Map.alter (Just . maybe [l] (l :)) tv m') mempty $ Bimap.toList lm
  --     forM_ (Map.elems ls_grouped) $ \ls -> xor ls

  --   getSolutions = \case
  --     Just 0 -> pure []
  --     i -> do
  --       Picosat.scopedSolutionWithAssumptions [] >>= \case
  --         Picosat.Solution ls -> do
  --           let found = mapMaybe (\l -> (l,) <$> Bimap.lookup l litMap) ls
  --           Picosat.addBaseClauses [[-l | (l, (tv, _)) <- found, tv `Set.member` tvs]]
  --           ((Subst $ Map.fromList $ map snd found) :) <$> getSolutions ((\x -> x - 1) <$> i)
  --         _ -> pure []

-- tryMatchPartial :: [InfernoType] -> TypeClass -> Solve (Maybe Subst)
-- tryMatchPartial tys (TypeClass _ tys2) =
--   (Just <$> evalSolveState (unifyMany [] tys tys2) varCount) `catchError` (\_ -> return Nothing)
--   where
--     varCount = case Set.lookupMax $ Set.unions $ map ftv $ tys ++ tys2 of
--       Just (TV i) -> i + 1
--       Nothing -> 0

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
-- filteredTypeClassSubstitutions :: Set.Set TypeClass -> [TypeClass] -> Map.Map TV (Set.Set InfernoType)
-- filteredTypeClassSubstitutions allClasses = \case
--   [] -> mempty
--   TypeClass nm tys : tcs -> do
--     let possibleMatchingInstances = Set.toList $ Set.filter (\(TypeClass nm' _) -> nm == nm') allClasses
--     case runIdentity $ runExceptT $ flip runReaderT allClasses $ catMaybes <$> forM possibleMatchingInstances (tryMatchPartial tys) of
--       Left _ -> filteredTypeClassSubstitutions allClasses tcs
--       Right subs' ->
--         let subs = [su | Subst su <- subs']
--             mergedSubs = foldr (Map.merge (Map.mapMissing $ \_k a -> Set.singleton a) Map.preserveMissing $ Map.zipWithMatched $ \_k a as -> Set.insert a as) mempty subs
--             finalMap = filteredTypeClassSubstitutions allClasses tcs
--          in Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched $ \_k as bs -> Set.intersection as bs) mergedSubs finalMap

-- encodeTypeClasses ::
--   (MonadState s f, HasType (Bimap.Bimap Int (TV, InfernoType)) s, HasType [[Int]] s, HasType Counter s) =>
--   Set.Set TypeClass ->
--   Map.Map TV (Set.Set InfernoType) ->
--   [TypeClass] ->
--   f ()
-- encodeTypeClasses allClasses filteredSubs = \case
--   [] -> pure ()
--   TypeClass nm tys : tcs -> do
--     let possibleMatchingInstances = Set.toList $ Set.filter (\(TypeClass nm' _) -> nm == nm') allClasses
--     case runIdentity $ runExceptT $ flip runReaderT allClasses $ catMaybes <$> forM possibleMatchingInstances (tryMatchPartial tys) of
--       Left _err -> encodeTypeClasses allClasses filteredSubs tcs
--       Right subs -> do
--         insts <- forM (filterSubs subs) $ \(Subst su) -> do
--           ls <-
--             concat
--               <$> forM
--                 tys
--                 ( \case
--                     TVar tv -> do
--                       let t' = su Map.! tv
--                       tvLit <- getLit (tv, t')
--                       freshLit <- newLit
--                       [tvLit] `iff` freshLit
--                       pure [freshLit]
--                     _ -> pure []
--                 )
--           freshLit <- newLit
--           ls `iff` freshLit
--           pure freshLit
--         xor insts
--         encodeTypeClasses allClasses filteredSubs tcs
--   where
--     filterSubs =
--       filter $ \(Subst su) ->
--         Map.foldrWithKey
--           ( \k v cond ->
--               cond && case Map.lookup k filteredSubs of
--                 Nothing -> False
--                 Just vs -> Set.member v vs
--           )
--           True
--           su

--     -- a_1 /\ ... /\ a_n -> b is equivalent to -a_1 \/ ... \/ -a_n \/ b
--     impl as b = addClause $ b : map (\a -> -a) as
--     -- a_1 /\ ... /\ a_n <-> b is equivalent to (a_1 /\ ... /\ a_n -> b) /\ (b -> a_1) /\ ... /\ (b -> a_n)
--     iff as b = do
--       as `impl` b
--       forM_ as $ \a -> [b] `impl` a

-- xor :: (MonadState s m, HasType [[Int]] s) => [Int] -> m ()
-- xor ls =
--   do
--     addClause ls
--     go ls
--   where
--     go [] = pure ()
--     go (x : xs) = do
--       forM_ xs $ \y -> addClause [-x, -y]
--       go xs

-- bind :: [TypeError SourcePos] -> TV -> InfernoType -> SolveState Int Subst
-- bind err a t
--   | t == TVar a = return emptySubst
--   | occursCheck a t = throwError [InfiniteType a t loc | loc <- getLocFromErrs err]
--   | otherwise = return (Subst $ Map.singleton a t)

-- occursCheck :: (Substitutable a) => TV -> a -> Bool
-- occursCheck a t = a `Set.member` ftv t
