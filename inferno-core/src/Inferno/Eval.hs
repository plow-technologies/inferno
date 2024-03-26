{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.Eval where

import Control.Monad.Catch (MonadCatch, MonadThrow (throwM), try)
import Control.Monad.Except (forM)
import Control.Monad.Reader (ask, local)
import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import Data.Tuple.Extra (fst3)
import Inferno.Eval.Error
  ( EvalError (AssertionFailed, RuntimeError),
  )
import Inferno.Module.Builtin (enumBoolHash)
import Inferno.Types.Syntax
  ( BaseType (..),
    Expr (..),
    ExtIdent (..),
    Ident (..),
    ImplExpl (..),
    InfernoType (TBase),
    Lit (LDouble, LHex, LInt, LText),
    Pat (..),
    tListToList,
    toEitherList,
  )
import Inferno.Types.Value
  ( ImplEnvM,
    Value (..),
    runImplEnvM,
  )
import Inferno.Types.VersionControl (VCObjectHash)
import Inferno.Utils.Prettyprinter (renderPretty)
import Prettyprinter
  ( LayoutOptions (LayoutOptions),
    PageWidth (Unbounded),
    Pretty (pretty),
    layoutPretty,
  )
import Prettyprinter.Render.Text (renderStrict)

-- | Evaluation environment: (localEnv, pinnedEnv).
-- The pinnedEnv contains functions in the prelude, and their definitions are either
-- inferno expressions or values (wrapped Haskell functions or direct VFun definitions).
type TermEnv hash c m a = (Map.Map ExtIdent (Value c m), Map.Map hash (Either (Expr (Maybe VCObjectHash) a) (Value c m)))

emptyTmenv :: TermEnv hash c m a
emptyTmenv = (Map.empty, Map.empty)

eval :: (MonadThrow m, Pretty c) => TermEnv VCObjectHash c (ImplEnvM m c) a -> Expr (Maybe VCObjectHash) a -> ImplEnvM m c (Value c (ImplEnvM m c))
eval env@(localEnv, pinnedEnv) expr = case expr of
  Lit_ (LInt k) -> return $
    VFun $ \case
      VTypeRep (TBase TInt) -> return $ VInt k
      VTypeRep (TBase TDouble) -> return $ VDouble $ fromIntegral k
      _ -> throwM $ RuntimeError "Invalid runtime rep for numeric constant."
  Lit_ (LDouble k) -> return $ VDouble k
  Lit_ (LHex w) -> return $ VWord64 w
  Lit_ (LText t) -> return $ VText t
  InterpolatedString_ es -> do
    res <- forM (toEitherList es) $ either (return . VText) (\(_, e, _) -> eval env e)
    return $ VText $ Text.concat $ map toText res
    where
      toText (VText t) = t
      toText e = renderStrict $ layoutPretty (LayoutOptions Unbounded) $ pretty e
  Array_ es ->
    foldrM (\(e, _) vs -> eval env e <&> (: vs)) [] es <&> VArray
  ArrayComp_ e srcs mCond -> do
    vals <- sequence' env srcs
    VArray <$> case mCond of
      Nothing ->
        forM vals $ \vs ->
          let nenv = foldr (uncurry Map.insert) localEnv vs in eval (nenv, pinnedEnv) e
      Just (_, cond) ->
        catMaybes
          <$> forM
            vals
            ( \vs -> do
                let nenv = foldr (uncurry Map.insert) localEnv vs
                eval (nenv, pinnedEnv) cond >>= \case
                  VEnum hash "true" ->
                    if hash == enumBoolHash
                      then Just <$> eval (nenv, pinnedEnv) e
                      else throwM $ RuntimeError "failed to match with a bool"
                  VEnum hash "false" ->
                    if hash == enumBoolHash
                      then return Nothing
                      else throwM $ RuntimeError "failed to match with a bool"
                  _ -> throwM $ RuntimeError "failed to match with a bool"
            )
    where
      sequence' :: (MonadThrow m, Pretty c) => TermEnv VCObjectHash c (ImplEnvM m c) a -> NonEmpty (a, Ident, a, Expr (Maybe VCObjectHash) a, Maybe a) -> ImplEnvM m c [[(ExtIdent, Value c (ImplEnvM m c))]]
      sequence' env'@(localEnv', pinnedEnv') = \case
        (_, Ident x, _, e_s, _) :| [] -> do
          eval env' e_s >>= \case
            VArray vals -> return $ map ((: []) . (ExtIdent $ Right x,)) vals
            _ -> throwM $ RuntimeError "failed to match with an array"
        (_, Ident x, _, e_s, _) :| (r : rs) -> do
          eval env' e_s >>= \case
            VArray vals ->
              concat
                <$> forM
                  vals
                  ( \v -> do
                      res <- sequence' (Map.insert (ExtIdent $ Right x) v localEnv', pinnedEnv') (r :| rs)
                      return $ map ((ExtIdent $ Right x, v) :) res
                  )
            _ -> throwM $ RuntimeError "failed to match with an array"
  Enum_ (Just hash) _ i -> return $ VEnum hash i
  Enum_ Nothing _ _ -> throwM $ RuntimeError "All enums must be pinned"
  Var_ (Just hash) _ x ->
    case Map.lookup hash pinnedEnv of
      Just (Left e) -> eval env e
      Just (Right v) -> return v
      Nothing -> throwM $ RuntimeError $ show x <> "(" <> show hash <> ") not found in the pinned env"
  Var_ Nothing _ (Expl x) -> do
    case Map.lookup x localEnv of
      Just v -> return v
      Nothing -> throwM $ RuntimeError $ show x <> " not found in the unpinned env"
  Var_ Nothing _ (Impl x) -> do
    implEnv <- ask
    case Map.lookup x implEnv of
      Just v -> return v
      Nothing -> throwM $ RuntimeError $ show x <> " not found in the implicit env"
  OpVar_ (Just hash) _ x ->
    case Map.lookup hash pinnedEnv of
      Just (Left e) -> eval env e
      Just (Right v) -> return v
      Nothing -> throwM $ RuntimeError $ show x <> "(" <> show hash <> ") not found in the pinned env"
  OpVar_ Nothing _ (Ident x) -> do
    case Map.lookup (ExtIdent $ Right x) localEnv of
      Just v -> return v
      Nothing -> throwM $ RuntimeError $ show x <> " not found in env"
  TypeRep_ t -> pure $ VTypeRep t
  Op_ _ Nothing _ op _ -> throwM $ RuntimeError $ show op <> " should be pinned"
  Op_ a (Just hash) _ns op b -> do
    a' <- eval env a
    b' <- eval env b
    vF <- case Map.lookup hash pinnedEnv of
      Nothing -> throwM $ RuntimeError $ show op <> "(" <> show hash <> ") not found in the pinned env"
      Just (Left e) -> eval env e
      Just (Right v) -> pure v
    case vF of
      VFun f ->
        f a' >>= \case
          VFun f' -> f' b'
          _ -> throwM $ RuntimeError $ show op <> " not bound to a binary function in env"
      _ -> throwM $ RuntimeError $ show op <> " not bound to a function in env"
  PreOp_ Nothing _ op _ -> throwM $ RuntimeError $ show op <> " should be pinned"
  PreOp_ (Just hash) _ns op a -> do
    a' <- eval env a
    vF <- case Map.lookup hash pinnedEnv of
      Nothing -> throwM $ RuntimeError $ show op <> "(" <> show hash <> ") not found in the pinned env"
      Just (Left e) -> eval env e
      Just (Right v) -> pure v
    case vF of
      VFun f -> f a'
      _ -> throwM $ RuntimeError $ show op <> " not bound to a function in env"
  Lam_ args body -> go localEnv $ toList args
    where
      go nenv = \case
        [] -> eval (nenv, pinnedEnv) body
        (_, Just x) : xs ->
          return $ VFun $ \arg -> go (Map.insert x arg nenv) xs
        (_, Nothing) : xs -> return $ VFun $ \_arg -> go nenv xs
  App_ fun arg ->
    eval env fun >>= \case
      VFun f -> do
        argv <- eval env arg
        f argv
      _ -> throwM $ RuntimeError "failed to match with a function"
  LetAnnot_ x e body -> do
    e' <- eval env e
    let nenv = Map.insert x e' localEnv
    eval (nenv, pinnedEnv) body
  Let_ (Expl x) e body -> do
    e' <- eval env e
    let nenv = Map.insert x e' localEnv
    eval (nenv, pinnedEnv) body
  Let_ (Impl x) e body -> do
    e' <- eval env e
    local (Map.insert x e') $ eval env body
  If_ cond tr fl ->
    eval env cond >>= \case
      VEnum hash "true" ->
        if hash == enumBoolHash
          then eval env tr
          else throwM $ RuntimeError "failed to match with a bool"
      VEnum hash "false" ->
        if hash == enumBoolHash
          then eval env fl
          else throwM $ RuntimeError "failed to match with a bool"
      _ -> throwM $ RuntimeError "failed to match with a bool"
  Tuple_ es ->
    foldrM (\(e, _) vs -> eval env e <&> (: vs)) [] (tListToList es) <&> VTuple
  Record_ fs -> do
    valMap <- foldrM (\(f, e, _) vs -> eval env e >>= \v -> return ((f, v) : vs)) [] fs
    return $ VRecord $ Map.fromList valMap
  RecordField_ (Ident r) f -> do
    case Map.lookup (ExtIdent $ Right r) localEnv of
      Just (VRecord fs) -> do
        case Map.lookup f fs of
          Just v -> return v
          Nothing -> throwM $ RuntimeError "record field not found"
      Just _ -> throwM $ RuntimeError "failed to match with a record"
      Nothing -> throwM $ RuntimeError $ show (ExtIdent $ Right r) <> " not found in the unpinned env"
  One_ e -> eval env e <&> VOne
  Empty_ -> return VEmpty
  Assert_ cond e ->
    eval env cond >>= \case
      VEnum hash "false" ->
        if hash == enumBoolHash
          then throwM AssertionFailed
          else throwM $ RuntimeError "failed to match with a bool"
      VEnum hash "true" ->
        if hash == enumBoolHash
          then eval env e
          else throwM $ RuntimeError "failed to match with a bool"
      _ -> throwM $ RuntimeError "failed to match with a bool"
  Case_ e pats -> do
    v <- eval env e
    matchAny v pats
    where
      matchAny v ((_, p, _, body) :| []) = case match v p of
        Just nenv ->
          -- (<>) is left biased so this will correctly override any shadowed vars from nenv onto env
          eval (nenv <> env) body
        Nothing -> throwM $ RuntimeError $ "non-exhaustive patterns in case detected in " <> Text.unpack (renderPretty v)
      matchAny v ((_, p, _, body) :| (r : rs)) = case match v p of
        Just nenv -> eval (nenv <> env) body
        Nothing -> matchAny v (r :| rs)

      match v p = case (v, p) of
        (_, PVar _ (Just (Ident x))) -> Just (Map.singleton (ExtIdent $ Right x) v, mempty)
        (_, PVar _ Nothing) -> Just mempty
        (VEnum h1 i1, PEnum _ (Just h2) _ i2) ->
          if h1 == h2 && i1 == i2
            then Just mempty
            else Nothing
        (VInt i1, PLit _ (LInt i2)) ->
          if i1 == i2
            then Just mempty
            else Nothing
        (VDouble d1, PLit _ (LDouble d2)) ->
          if d1 == d2
            then Just mempty
            else Nothing
        (VText t1, PLit _ (LText t2)) ->
          if t1 == t2
            then Just mempty
            else Nothing
        (VWord64 h1, PLit _ (LHex h2)) ->
          if h1 == h2
            then Just mempty
            else Nothing
        (VOne v', POne _ p') -> match v' p'
        (VEmpty, PEmpty _) -> Just mempty
        (VArray vs, PArray _ ps _) -> matchElems vs ps
        (VTuple vs, PTuple _ ps _) -> matchElems vs $ tListToList ps
        (VRecord vs, PRecord _ ps _) ->
          if fs == fs'
            then matchElems vs' ps'
            else Nothing
          where
            (fs, vs') = unzip $ Map.toAscList vs
            (fs', ps') = unzip $ map (\(f, p', l) -> (f, (p', l))) $ sortOn fst3 ps
        _ -> Nothing

      matchElems [] [] = Just mempty
      matchElems (v' : vs) ((p', _) : ps) = do
        env1 <- match v' p'
        env2 <- matchElems vs ps
        -- since variables in patterns must be linear,
        -- env1 and env2 should not overlap, so it doesn't
        -- matter which way around we combine
        return $ env1 <> env2
      matchElems _ _ = Nothing
  CommentAbove _ e -> eval env e
  CommentAfter e _ -> eval env e
  CommentBelow e _ -> eval env e
  Bracketed_ e -> eval env e
  RenameModule_ _ _ e -> eval env e
  OpenModule_ _ _ e -> eval env e

-- | Evaluate an expression with the provided environments.
--   If an 'EvalError' exception is thrown during evaluation, it will be
--   caught and a 'Left' result will be returned.
runEvalM ::
  (MonadThrow m, MonadCatch m, Pretty c) =>
  -- | Environment.
  TermEnv VCObjectHash c (ImplEnvM m c) a ->
  -- | Implicit environment.
  Map.Map ExtIdent (Value c (ImplEnvM m c)) ->
  -- | Expression to evaluate.
  Expr (Maybe VCObjectHash) a ->
  m (Either EvalError (Value c (ImplEnvM m c)))
runEvalM env implicitEnv ex =
  try $ runImplEnvM implicitEnv $ eval env ex
