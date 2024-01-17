{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.Eval where

import Control.Monad.Catch (MonadCatch, MonadThrow (throwM), try)
import Control.Monad.Except (forM)
import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
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
  ( Value
      ( VArray,
        VDouble,
        VEmpty,
        VEnum,
        VFun,
        VInt,
        VOne,
        VText,
        VTuple,
        VTypeRep,
        VWord64
      ),
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

type TermEnv hash c m = (Map.Map ExtIdent (Value c m), Map.Map hash (Value c m))

emptyTmenv :: TermEnv hash c m
emptyTmenv = (Map.empty, Map.empty)

eval ::
  (MonadThrow m, Pretty c) =>
  TermEnv VCObjectHash c m ->
  Map.Map ExtIdent (Value c m) ->
  Expr (Maybe VCObjectHash) a ->
  m (Value c m)
eval env@(localEnv, pinnedEnv) implEnv expr = case expr of
  Lit_ (LInt k) -> return $
    VFun $ \_implEnv -> \case
      VTypeRep (TBase TInt) -> return $ VInt k
      VTypeRep (TBase TDouble) -> return $ VDouble $ fromIntegral k
      _ -> throwM $ RuntimeError "Invalid runtime rep for numeric constant."
  Lit_ (LDouble k) -> return $ VDouble k
  Lit_ (LHex w) -> return $ VWord64 w
  Lit_ (LText t) -> return $ VText t
  InterpolatedString_ es -> do
    res <- forM (toEitherList es) $ either (return . VText) (\(_, e, _) -> eval env implEnv e)
    return $ VText $ Text.concat $ map toText res
    where
      toText (VText t) = t
      toText e = renderStrict $ layoutPretty (LayoutOptions Unbounded) $ pretty e
  Array_ es ->
    foldrM (\(e, _) vs -> eval env implEnv e >>= return . (: vs)) [] es >>= return . VArray
  ArrayComp_ e srcs mCond -> do
    vals <- sequence' env srcs
    VArray <$> case mCond of
      Nothing ->
        forM vals $ \vs ->
          let nenv = foldr (uncurry Map.insert) localEnv vs in eval (nenv, pinnedEnv) implEnv e
      Just (_, cond) ->
        catMaybes
          <$> ( forM vals $ \vs -> do
                  let nenv = foldr (uncurry Map.insert) localEnv vs
                  eval (nenv, pinnedEnv) implEnv cond >>= \case
                    VEnum hash "true" ->
                      if hash == enumBoolHash
                        then Just <$> (eval (nenv, pinnedEnv) implEnv e)
                        else throwM $ RuntimeError "failed to match with a bool"
                    VEnum hash "false" ->
                      if hash == enumBoolHash
                        then return Nothing
                        else throwM $ RuntimeError "failed to match with a bool"
                    _ -> throwM $ RuntimeError "failed to match with a bool"
              )
    where
      sequence' env'@(localEnv', pinnedEnv') = \case
        (_, Ident x, _, e_s, _) :| [] -> do
          eval env' implEnv e_s >>= \case
            VArray vals -> return $ map ((: []) . (ExtIdent $ Right x,)) vals
            _ -> throwM $ RuntimeError "failed to match with an array"
        (_, Ident x, _, e_s, _) :| (r : rs) -> do
          eval env' implEnv e_s >>= \case
            VArray vals ->
              concat
                <$> ( forM vals $ \v -> do
                        res <- sequence' (Map.insert (ExtIdent $ Right x) v localEnv', pinnedEnv') (r :| rs)
                        return $ map ((ExtIdent $ Right x, v) :) res
                    )
            _ -> throwM $ RuntimeError "failed to match with an array"
  Enum_ (Just hash) _ i -> return $ VEnum hash i
  Enum_ Nothing _ _ -> throwM $ RuntimeError "All enums must be pinned"
  Var_ (Just hash) _ x ->
    case Map.lookup hash pinnedEnv of
      Just v -> return v
      Nothing -> throwM $ RuntimeError $ show x <> "(" <> show hash <> ") not found in the pinned env"
  Var_ Nothing _ (Expl x) -> do
    case Map.lookup x localEnv of
      Just v -> return v
      Nothing -> throwM $ RuntimeError $ show x <> " not found in the unpinned env"
  Var_ Nothing _ (Impl x) -> do
    case Map.lookup x implEnv of
      Just v -> return v
      Nothing -> throwM $ RuntimeError $ show x <> " not found in the implicit env"
  OpVar_ (Just hash) _ x ->
    case Map.lookup hash pinnedEnv of
      Just v -> return v
      Nothing -> throwM $ RuntimeError $ show x <> "(" <> show hash <> ") not found in the pinned env"
  OpVar_ Nothing _ (Ident x) -> do
    case Map.lookup (ExtIdent $ Right x) localEnv of
      Just v -> return v
      Nothing -> throwM $ RuntimeError $ show x <> " not found in env"
  TypeRep_ t -> pure $ VTypeRep t
  Op_ _ Nothing _ op _ -> throwM $ RuntimeError $ show op <> " should be pinned"
  Op_ a (Just hash) _ns op b -> do
    a' <- eval env implEnv a
    b' <- eval env implEnv b
    case Map.lookup hash pinnedEnv of
      Nothing -> throwM $ RuntimeError $ show op <> "(" <> show hash <> ") not found in the pinned env"
      Just (VFun f) ->
        f implEnv a' >>= \case
          VFun f' -> f' implEnv b'
          _ -> throwM $ RuntimeError $ show op <> " not bound to a binary function in env"
      Just _ -> throwM $ RuntimeError $ show op <> " not bound to a function in env"
  PreOp_ Nothing _ op _ -> throwM $ RuntimeError $ show op <> " should be pinned"
  PreOp_ (Just hash) _ns op a -> do
    a' <- eval env implEnv a
    case Map.lookup hash pinnedEnv of
      Nothing -> throwM $ RuntimeError $ show op <> "(" <> show hash <> ") not found in the pinned env"
      Just (VFun f) -> f implEnv a'
      Just _ -> throwM $ RuntimeError $ show op <> " not bound to a function in env"
  Lam_ args body -> go localEnv $ toList args
    where
      go nenv = \case
        [] -> eval (nenv, pinnedEnv) implEnv body
        (_, Just x) : xs ->
          return $ VFun $ \_implEnv arg -> go (Map.insert x arg nenv) xs
        (_, Nothing) : xs -> return $ VFun $ \_implEnv _arg -> go nenv xs
  App_ fun arg -> do
    eval env implEnv fun >>= \case
      VFun f -> do
        argv <- eval env implEnv arg
        f implEnv argv
      _ -> throwM $ RuntimeError "failed to match with a function"
  LetAnnot_ x e body -> do
    e' <- eval env implEnv e
    let nenv = Map.insert x e' localEnv
    eval (nenv, pinnedEnv) implEnv body
  Let_ (Expl x) e body -> do
    e' <- eval env implEnv e
    let nenv = Map.insert x e' localEnv
    eval (nenv, pinnedEnv) implEnv body
  Let_ (Impl x) e body -> do
    e' <- eval env implEnv e
    eval env (Map.insert x e' implEnv) body
  If_ cond tr fl ->
    eval env implEnv cond >>= \case
      VEnum hash "true" ->
        if hash == enumBoolHash
          then eval env implEnv tr
          else throwM $ RuntimeError "failed to match with a bool"
      VEnum hash "false" ->
        if hash == enumBoolHash
          then eval env implEnv fl
          else throwM $ RuntimeError "failed to match with a bool"
      _ -> throwM $ RuntimeError "failed to match with a bool"
  Tuple_ es ->
    foldrM (\(e, _) vs -> eval env implEnv e >>= return . (: vs)) [] (tListToList es) >>= return . VTuple
  One_ e -> eval env implEnv e >>= return . VOne
  Empty_ -> return $ VEmpty
  Assert_ cond e ->
    eval env implEnv cond >>= \case
      VEnum hash "false" ->
        if hash == enumBoolHash
          then throwM AssertionFailed
          else throwM $ RuntimeError "failed to match with a bool"
      VEnum hash "true" ->
        if hash == enumBoolHash
          then eval env implEnv e
          else throwM $ RuntimeError "failed to match with a bool"
      _ -> throwM $ RuntimeError "failed to match with a bool"
  Case_ e pats -> do
    v <- eval env implEnv e
    matchAny v pats
    where
      matchAny v ((_, p, _, body) :| []) = case match v p of
        Just nenv ->
          -- (<>) is left biased so this will correctly override any shadowed vars from nenv onto env
          eval (nenv <> env) implEnv body
        Nothing -> throwM $ RuntimeError $ "non-exhaustive patterns in case detected in " <> (Text.unpack $ renderPretty v)
      matchAny v ((_, p, _, body) :| (r : rs)) = case match v p of
        Just nenv -> eval (nenv <> env) implEnv body
        Nothing -> matchAny v (r :| rs)

      match v p = case (v, p) of
        (_, PVar _ (Just (Ident x))) -> Just $ (Map.singleton (ExtIdent $ Right x) v, mempty)
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
  CommentAbove _ e -> eval env implEnv e
  CommentAfter e _ -> eval env implEnv e
  CommentBelow e _ -> eval env implEnv e
  Bracketed_ e -> eval env implEnv e
  RenameModule_ _ _ e -> eval env implEnv e
  OpenModule_ _ _ e -> eval env implEnv e

-- | Evaluate an expression with the provided environments.
--   If an 'EvalError' exception is thrown during evaluation, it will be
--   caught and a 'Left' result will be returned.
runEvalM ::
  (MonadThrow m, MonadCatch m, Pretty c) =>
  -- | Environment.
  TermEnv VCObjectHash c m ->
  -- | Implicit environment.
  Map.Map ExtIdent (Value c m) ->
  -- | Expression to evaluate.
  Expr (Maybe VCObjectHash) a ->
  m (Either EvalError (Value c m))
runEvalM env implicitEnv ex =
  try $ eval env implicitEnv ex
