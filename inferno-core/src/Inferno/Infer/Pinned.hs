{-# LANGUAGE TypeFamilies #-}

module Inferno.Infer.Pinned
  ( pinExpr,
    insertHardcodedModule,
    insertBuiltinModule,
    openModule,
  )
where

import Control.Monad (foldM, forM, when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (get, put, runStateT)
import Data.Functor.Foldable (cata)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Inferno.Infer.Error (TypeError (..))
import Inferno.Module.Builtin (builtinModule)
import Inferno.Types.Module (Module (..))
import Inferno.Types.Syntax (Expr (..), ExtIdent (..), Ident (..), ImplExpl (..), Import (..), ModuleName (..), Pat (..), PatF (..), Scoped (..), blockPosition, elementPosition)
import Inferno.Types.Type (Namespace (..))
import Inferno.Types.VersionControl (Pinned (..), VCObjectHash)
import Text.Megaparsec (SourcePos (..))

insertIntoLocalScope ::
  Map Namespace (Pinned a) ->
  Map (Scoped ModuleName) (Map Namespace (Pinned a)) ->
  Map (Scoped ModuleName) (Map Namespace (Pinned a))
insertIntoLocalScope m moduleMap =
  Map.alter (Just . addModuleToLocalScope) LocalScope moduleMap
  where
    addModuleToLocalScope maybeMap = case maybeMap of
      Nothing -> m
      Just m' -> m `Map.union` m'

insertHardcodedModule ::
  ModuleName ->
  Map Namespace (Pinned a) ->
  Map (Scoped ModuleName) (Map Namespace (Pinned a)) ->
  Map (Scoped ModuleName) (Map Namespace (Pinned a))
insertHardcodedModule modNm m moduleMap =
  insertIntoLocalScope (Map.filterWithKey isModuleNamespace m) $
    Map.insert (Scope modNm) m moduleMap
  where
    isModuleNamespace :: Namespace -> p -> Bool
    isModuleNamespace k _v = case k of
      ModuleNamespace _ -> True
      _ -> False

insertBuiltinModule ::
  Map (Scoped ModuleName) (Map Namespace (Pinned a)) ->
  Map (Scoped ModuleName) (Map Namespace (Pinned a))
insertBuiltinModule =
  openModule "Builtin"
    . insertHardcodedModule "Builtin" (Map.map Builtin builtinTysToHash)
  where
    builtinTysToHash :: Map Namespace VCObjectHash
    Module {moduleObjects = (builtinTysToHash, _, _)} = builtinModule

openModule ::
  ModuleName ->
  Map (Scoped ModuleName) (Map Namespace (Pinned a)) ->
  Map (Scoped ModuleName) (Map Namespace (Pinned a))
openModule modNm moduleMap = case Map.lookup (Scope modNm) moduleMap of
  Nothing -> error $ "openModule: Module " <> show modNm <> " does not exist."
  Just m -> insertIntoLocalScope m moduleMap

lookupName ::
  (MonadError [TypeError SourcePos] m, Eq a) =>
  (SourcePos, SourcePos) ->
  Scoped ModuleName ->
  Namespace ->
  Map (Scoped ModuleName) (Map Namespace (Pinned a)) ->
  m (Pinned a)
lookupName loc modNm ns m = case Map.lookup modNm m of
  Just m' -> case Map.lookup ns m' of
    Just r -> pure r
    Nothing -> throwError [UnboundNameInNamespace modNm (Right ns) loc]
  Nothing -> case modNm of
    Scope nm -> throwError [ModuleDoesNotExist nm loc]
    LocalScope -> throwError [UnboundNameInNamespace modNm (Right ns) loc]

pinPat :: (MonadError [TypeError SourcePos] m, Eq a) => Map (Scoped ModuleName) (Map Namespace (Pinned a)) -> Pat h SourcePos -> m (Pat (Pinned a) SourcePos)
pinPat m pat =
  let patPos = blockPosition pat
   in case pat of
        PVar p i -> pure $ PVar p i
        PEnum p _ modNm x -> do
          hash <- lookupName patPos modNm (EnumNamespace x) m
          pure $ PEnum p hash modNm x
        PLit p l -> pure $ PLit p l
        POne p e -> POne p <$> pinPat m e
        PEmpty p -> pure $ PEmpty p
        PTuple p1 es p2 -> do
          es' <- mapM (\(e, p3) -> (,p3) <$> pinPat m e) es
          pure $ PTuple p1 es' p2
        PCommentAbove c e -> PCommentAbove c <$> pinPat m e
        PCommentAfter e c -> (\e' -> PCommentAfter e' c) <$> pinPat m e
        PCommentBelow e c -> (\e' -> PCommentBelow e' c) <$> pinPat m e

-- pinExpr ::
--   (MonadError [TypeError SourcePos] m, Eq a) =>
--   Map ModuleName (Map Namespace (Pinned a)) ->
--   Expr h SourcePos ->
--   m (Expr (Pinned a) SourcePos)
-- pinExpr nameMap = pinExpr nameMapWithBuiltin
--   where
--     Module {moduleObjects = (builtinTysToHash, _, _)} = builtinModule

--     nameMapWithBuiltin =
--       let moduleNames = Map.filterWithKey isModNs $ Map.unions $ Map.elems nameMap
--        in Map.insert LocalScope (Map.map Builtin builtinTysToHash `Map.union` moduleNames) $
--             Map.mapKeysMonotonic Scope $ Map.insert "Builtin" (Map.map Builtin builtinTysToHash) nameMap

patVars :: Pat hash pos -> [Ident]
patVars p =
  flip cata p $
    \case
      PVarF _ (Just v) -> [v]
      rest -> foldr (++) [] rest

isModNs :: Namespace -> p -> Bool
isModNs k _v = case k of
  ModuleNamespace _ -> True
  _ -> False

pinExpr :: (MonadError [TypeError SourcePos] m, Eq a) => Map (Scoped ModuleName) (Map Namespace (Pinned a)) -> Expr h SourcePos -> m (Expr (Pinned a) SourcePos)
pinExpr m expr =
  let exprPos = blockPosition expr
      insertLocal k m' = Map.alter (alterFun (FunNamespace k) Local) LocalScope m'
      alterFun k v = \case
        Just m' -> Just $ Map.insert k v m'
        Nothing -> Just $ Map.singleton k v
   in case expr of
        Lit p l -> pure $ Lit p l
        Var p _hash modNm (Impl x) -> pure $ Var p Local modNm (Impl x)
        Var p _hash modNm i@(Expl (ExtIdent (Left _))) -> pure $ Var p Local modNm i
        Var p _hash modNm i@(Expl (ExtIdent (Right x))) -> do
          hash <- lookupName exprPos modNm (FunNamespace $ Ident x) m
          pure $ Var p hash modNm i
        OpVar p _hash modNm x -> do
          hash <- lookupName exprPos modNm (OpNamespace x) m
          pure $ OpVar p hash modNm x
        TypeRep p t -> pure $ TypeRep p t
        Enum p _hash modNm x -> do
          hash <- lookupName exprPos modNm (EnumNamespace x) m
          pure $ Enum p hash modNm x
        InterpolatedString p1 xs p2 -> do
          xs' <- mapM (\(p3, e, p4) -> (\e' -> (p3, e', p4)) <$> pinExpr m e) xs
          pure $ InterpolatedString p1 xs' p2
        Array p1 es p2 -> do
          es' <- mapM (\(e, p3) -> (,p3) <$> pinExpr m e) es
          pure $ Array p1 es' p2
        ArrayComp p1 e p2 sels cond p3 -> do
          (sels', m') <- flip runStateT m $
            forM sels $ \(p4, i, p5, e1, p6) -> do
              currentM <- get
              e1' <- pinExpr currentM e1
              put $ insertLocal i currentM
              pure $ (p4, i, p5, e1', p6)

          cond' <- mapM (\(p4, e1) -> (p4,) <$> pinExpr m' e1) cond
          e' <- pinExpr m' e
          pure $ ArrayComp p1 e' p2 sels' cond' p3
        Lam p1 args p2 e -> do
          let m' =
                foldr
                  ( \(_, mIdent) m'' -> case mIdent of
                      Just (ExtIdent (Right i)) -> insertLocal (Ident i) m''
                      _ -> m''
                  )
                  m
                  args
          Lam p1 args p2 <$> pinExpr m' e
        App e1 e2 -> App <$> pinExpr m e1 <*> pinExpr m e2
        Let p1 loc x@(Expl (ExtIdent (Right i))) p2 e1 p3 e2 -> do
          e1' <- pinExpr m e1
          e2' <- pinExpr (insertLocal (Ident i) m) e2
          pure $ Let p1 loc x p2 e1' p3 e2'
        Let p1 loc x@(Expl (ExtIdent (Left _))) p2 e1 p3 e2 -> do
          e1' <- pinExpr m e1
          e2' <- pinExpr m e2
          pure $ Let p1 loc x p2 e1' p3 e2'
        Let p1 loc (Impl x) p2 e1 p3 e2 -> do
          e1' <- pinExpr m e1
          e2' <- pinExpr m e2
          pure $ Let p1 loc (Impl x) p2 e1' p3 e2'
        Op e1 p1 _ meta modNm op e2 -> do
          hash <- lookupName exprPos modNm (OpNamespace op) m
          (\e1' e2' -> Op e1' p1 hash meta modNm op e2')
            <$> pinExpr m e1
            <*> pinExpr m e2
        PreOp loc _ meta modNm op e -> do
          hash <- lookupName exprPos modNm (FunNamespace op) m
          PreOp loc hash meta modNm op <$> pinExpr m e
        If p1 cond p2 tr p3 fl ->
          (\c t f -> If p1 c p2 t p3 f) <$> pinExpr m cond <*> pinExpr m tr <*> pinExpr m fl
        Tuple p1 es p2 -> do
          es' <- mapM (\(e, p3) -> (,p3) <$> pinExpr m e) es
          pure $ Tuple p1 es' p2
        Assert p1 cond p2 e ->
          (\cond' e' -> Assert p1 cond' p2 e')
            <$> pinExpr m cond
            <*> pinExpr m e
        Empty p -> pure $ Empty p
        One p e -> One p <$> pinExpr m e
        Case p1 e p2 patExprs p3 -> do
          e' <- pinExpr m e
          patExprs' <-
            mapM
              ( \(p4, pat, p5, e1) -> do
                  pat' <- pinPat m pat
                  let m' = foldr insertLocal m $ patVars pat
                  e1' <- pinExpr m' e1
                  pure $ (p4, pat', p5, e1')
              )
              patExprs
          pure $ Case p1 e' p2 patExprs' p3
        CommentAbove c e -> CommentAbove c <$> pinExpr m e
        CommentAfter e c -> (\e' -> CommentAfter e' c) <$> pinExpr m e
        CommentBelow e c -> (\e' -> CommentBelow e' c) <$> pinExpr m e
        Bracketed p1 e p2 -> (\e' -> Bracketed p1 e' p2) <$> pinExpr m e
        RenameModule l1 newNm l2 oldNm l3 e -> do
          hash <- lookupName exprPos LocalScope (ModuleNamespace oldNm) m
          when (Scope newNm `Map.member` m) $ throwError [ModuleNameTaken newNm $ elementPosition l1 newNm]
          case Map.lookup (Scope oldNm) m of
            Nothing -> throwError [ModuleDoesNotExist oldNm (l2, l3)]
            Just oldNmMod -> do
              let m' = Map.alter (alterFun (ModuleNamespace newNm) hash) LocalScope $ Map.insert (Scope newNm) oldNmMod m
              RenameModule l1 newNm l2 oldNm l3 <$> pinExpr m' e
        OpenModule p1 _mHash modNm@(ModuleName mn) imports p2 e -> do
          hash <- lookupName exprPos LocalScope (ModuleNamespace modNm) m
          let modPos = elementPosition p1 $ Ident mn
          case Map.lookup (Scope modNm) m of
            Nothing -> throwError [ModuleDoesNotExist modNm modPos]
            Just openMod' -> do
              let openMod = Map.filterWithKey (\k v -> not $ isModNs k v) openMod'
              let localM = fromMaybe mempty $ Map.lookup LocalScope m

              checkedImports <- case imports of
                [] -> pure $ openMod
                _ -> Map.fromList <$> (foldM (collectImports openMod modPos) [] $ map fst imports)

              let intersectionWithLocal = localM `Map.intersection` checkedImports
              when (not $ Map.null intersectionWithLocal) $ throwError [AmbiguousName modNm i modPos | i <- Map.keys checkedImports]

              OpenModule p1 hash modNm imports p2 <$> pinExpr (Map.insertWith Map.union LocalScope checkedImports m) e
          where
            collectImports openMod pos xs = \case
              IVar _ i -> do
                let k = FunNamespace i
                when (not $ k `Map.member` openMod) $ throwError [NameInModuleDoesNotExist modNm i pos]
                return $ (k, openMod Map.! k) : xs
              IOpVar _ i -> do
                let k = FunNamespace i
                when (not $ k `Map.member` openMod) $ throwError [NameInModuleDoesNotExist modNm i pos]
                return $ (k, openMod Map.! k) : xs
              IEnum _ _ i -> do
                let k = TypeNamespace i
                when (not $ k `Map.member` openMod) $ throwError [NameInModuleDoesNotExist modNm i pos]
                let enumHash = openMod Map.! k
                return $
                  (Map.toList $ Map.filter (\h -> h == enumHash) openMod) ++ xs
              ICommentAbove _ x' -> collectImports openMod pos xs x'
              ICommentAfter x' _ -> collectImports openMod pos xs x'
              ICommentBelow x' _ -> collectImports openMod pos xs x'
