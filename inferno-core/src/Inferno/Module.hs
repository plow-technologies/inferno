{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Inferno.Module
  ( Module (..),
    PinnedModule,
    BuiltinModuleHash (..),
    BuiltinFunHash (..),
    BuiltinEnumHash (..),
    buildPinnedQQModules,
    combineTermEnvs,
    pinnedModuleNameToHash,
    pinnedModuleHashToTy,
    pinnedModuleTerms,
    ToValue (..),
  )
where

import Control.Monad (foldM)
import Control.Monad.Catch (MonadThrow (..))
import Data.Bifunctor (bimap)
import Data.Foldable (foldl')
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import Inferno.Eval (TermEnv)
import Inferno.Infer (inferExpr)
import Inferno.Infer.Env (Namespace (..), TypeMetadata (..))
import Inferno.Infer.Pinned (pinExpr)
import qualified Inferno.Infer.Pinned as Pinned
import Inferno.Module.Cast (ToValue (..))
import Inferno.Parse (OpsTable, TopLevelDefn (..))
import Inferno.Types.Module
  ( BuiltinEnumHash (..),
    BuiltinFunHash (..),
    BuiltinModuleHash (..),
    Module (..),
    PinnedModule,
    pinnedModuleHashToTy,
    pinnedModuleNameToHash,
    pinnedModuleTerms,
  )
import Inferno.Types.Syntax
  ( Expr (..),
    ExtIdent (..),
    Ident (..),
    ImplExpl (..),
    ModuleName,
    Scoped (..),
    SigVar (..),
    sigVarToExpr,
  )
import Inferno.Types.Type
  ( BaseType (TEnum),
    ImplType (..),
    InfernoType (TBase),
    TCScheme (..),
  )
import Inferno.Types.Value (ImplEnvM, Value)
import Inferno.Types.VersionControl (Pinned (..), VCObjectHash, pinnedToMaybe, vcHash)
import Prettyprinter (Pretty)
import Text.Megaparsec (SourcePos)

combineTermEnvs ::
  MonadThrow m =>
  Map.Map ModuleName (PinnedModule (ImplEnvM m c (TermEnv VCObjectHash c (ImplEnvM m c) a))) ->
  ImplEnvM m c (TermEnv VCObjectHash c (ImplEnvM m c) a)
combineTermEnvs modules = foldM (\env m -> (env <>) <$> pinnedModuleTerms m) mempty $ Map.elems modules

buildPinnedQQModules ::
  (MonadThrow m, Pretty c) =>
  [(ModuleName, OpsTable, [TopLevelDefn (Either (TCScheme, ImplEnvM m c (Value c (ImplEnvM m c))) (Maybe TCScheme, Expr () SourcePos))])] ->
  Map.Map ModuleName (PinnedModule (ImplEnvM m c (TermEnv VCObjectHash c (ImplEnvM m c) ())))
buildPinnedQQModules modules =
  snd $
    foldl'
      ( \(alreadyPinnedModulesMap, alreadyBuiltModules) (moduleNm, opsTable, sigs) ->
          -- first build the new module
          let newMod =
                buildModule alreadyPinnedModulesMap alreadyBuiltModules sigs $
                  Module
                    { moduleName = moduleNm,
                      moduleOpsTable = opsTable,
                      moduleTypeClasses = mempty,
                      moduleObjects = (Map.singleton (ModuleNamespace moduleNm) $ vcHash $ BuiltinModuleHash moduleNm, mempty, pure mempty)
                    }
           in -- then insert it into the temporary module pin map as well as the final module map
              ( Pinned.insertHardcodedModule moduleNm (Map.map Builtin $ pinnedModuleNameToHash newMod) alreadyPinnedModulesMap,
                Map.insert moduleNm newMod alreadyBuiltModules
              )
      )
      mempty
      modules
  where
    buildModule ::
      (MonadThrow m, Pretty c) =>
      Map.Map (Scoped ModuleName) (Map.Map Namespace (Pinned VCObjectHash)) ->
      Map.Map ModuleName (PinnedModule (ImplEnvM m c (TermEnv VCObjectHash c (ImplEnvM m c) ()))) ->
      [TopLevelDefn (Either (TCScheme, ImplEnvM m c (Value c (ImplEnvM m c))) (Maybe TCScheme, Expr () SourcePos))] ->
      PinnedModule (ImplEnvM m c (TermEnv VCObjectHash c (ImplEnvM m c) ())) ->
      PinnedModule (ImplEnvM m c (TermEnv VCObjectHash c (ImplEnvM m c) ()))
    buildModule _ _ [] m = m
    buildModule alreadyPinnedModulesMap alreadyBuiltModules (Signature {..} : xs) m@Module {moduleName, moduleObjects = (nsMap, tyMap, mTrmEnv)} =
      let sigVarToNamespace = \case
            SigVar n -> FunNamespace $ Ident n
            SigOpVar n -> OpNamespace $ Ident n
          (sig, ns, hsh, mTrmEnv') = case def of
            Left (sig', mVal) ->
              let ns' = sigVarToNamespace name
                  hsh' = vcHash $ BuiltinFunHash (sigVarToExpr LocalScope name, sig)
               in (sig', ns', hsh', (\val (local, pinned) -> (local, Map.insert hsh (Right val) pinned)) <$> mVal <*> mTrmEnv)
            Right (mSig, expr) ->
              let pinMap =
                    Pinned.openModule moduleName $
                      Pinned.insertHardcodedModule
                        moduleName
                        (Map.map Builtin nsMap)
                        alreadyPinnedModulesMap
                  pinnedExpr = either (error . show) id $ pinExpr pinMap expr
                  inferEnv = Map.insert moduleName m $ alreadyBuiltModules
                  (pinnedExpr', sig') =
                    either (\err -> error $ "Could not infer the type of this expression: " <> show err) (\(e, typ, _) -> (e, typ)) $
                      inferExpr inferEnv $
                        pinnedExpr
                  ns' = sigVarToNamespace name
                  hsh' = vcHash $ BuiltinFunHash (sigVarToExpr LocalScope name, sig)
                  finalExpr = (bimap pinnedToMaybe (const ()) pinnedExpr')
               in case mSig of
                    Just sig''
                      | sig' /= sig'' ->
                          error $ "Type of " <> show name <> " does not matched inferred type " <> show sig'
                    _ ->
                      (sig', ns', hsh', (\(local, pinned) -> (local, Map.insert hsh (Left finalExpr) pinned)) <$> mTrmEnv)
       in buildModule alreadyPinnedModulesMap alreadyBuiltModules xs $
            m
              { moduleObjects =
                  ( Map.insert ns hsh nsMap,
                    Map.insert
                      hsh
                      TypeMetadata
                        { identExpr = sigVarToExpr (Scope moduleName) name,
                          docs = documentation,
                          ty = sig
                        }
                      tyMap,
                    mTrmEnv'
                  )
              }
    buildModule alreadyPinnedModulesMap alreadyBuiltModules (TypeClassInstance tCl : xs) m@Module {moduleTypeClasses = tCls} =
      buildModule alreadyPinnedModulesMap alreadyBuiltModules xs m {moduleTypeClasses = Set.insert tCl tCls}
    buildModule alreadyPinnedModulesMap alreadyBuiltModules (Export modNm : xs) Module {moduleName, moduleOpsTable = opsTable, moduleTypeClasses = tyCls, moduleObjects = (nsMap, tyMap, mTrmEnv)} =
      case Map.lookup modNm alreadyBuiltModules of
        Nothing -> error $ "buildModule: Module " <> show modNm <> " does not exist."
        Just Module {moduleOpsTable = opsTable', moduleTypeClasses = tyCls', moduleObjects = (nsMap', tyMap', mTrmEnv')} ->
          buildModule
            alreadyPinnedModulesMap
            alreadyBuiltModules
            xs
            Module
              { moduleName,
                moduleOpsTable = IntMap.unionWith (<>) opsTable opsTable',
                moduleTypeClasses = tyCls <> tyCls',
                moduleObjects = (nsMap <> nsMap', tyMap <> tyMap', mTrmEnv >>= \x -> mTrmEnv' >>= \y -> pure $ x <> y)
              }
    buildModule alreadyPinnedModulesMap alreadyBuiltModules (EnumDef doc nm cs : xs) m@Module {moduleObjects = (nsMap, tyMap, mTrmEnv)} =
      let enumTy = ForallTC [] Set.empty $ ImplType Map.empty $ TBase $ TEnum nm $ Set.fromList cs
          hsh = vcHash $ BuiltinEnumHash enumTy
          nms = TypeNamespace (Ident nm) : [EnumNamespace c | c <- cs]
       in buildModule alreadyPinnedModulesMap alreadyBuiltModules xs $
            m
              { moduleObjects =
                  ( Map.fromList [(n, hsh) | n <- nms] `Map.union` nsMap,
                    Map.insert
                      hsh
                      TypeMetadata
                        { identExpr = Var () () LocalScope (Expl $ ExtIdent $ Right "_"),
                          docs = doc,
                          ty = enumTy
                        }
                      tyMap,
                    mTrmEnv
                  )
              }
