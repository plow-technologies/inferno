{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Inferno.Module
  ( Module (..),
    PinnedModule,
    Prelude (..),
    BuiltinModuleHash (..),
    BuiltinFunHash (..),
    BuiltinEnumHash (..),
    baseOpsTable,
    moduleOpsTables,
    preludePinMap,
    preludeTermEnv,
    preludeNameToTypeMap,
    buildPinnedQQModules,
    combineTermEnvs,
    emptyPrelude,
    buildInitPrelude,
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
import Inferno.Module.Builtin (builtinModule)
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

data Prelude m c = Prelude
  { moduleMap :: Map.Map ModuleName (PinnedModule (TermEnv VCObjectHash c (ImplEnvM m c) ())),
    pinnedModuleMap :: Map.Map (Scoped ModuleName) (Map.Map Namespace (Pinned VCObjectHash))
    -- TODO is pinnedModuleMap not the same as the first component of the PinnedModule of moduleMap above?
  }

emptyPrelude :: Prelude m c
emptyPrelude = Prelude mempty mempty

baseOpsTable :: forall c m. Prelude m c -> OpsTable
baseOpsTable Prelude {moduleMap} =
  case Map.lookup "Base" moduleMap of
    Just (Module {moduleOpsTable = ops, moduleName = modNm}) ->
      -- TODO is 'Scope modNm' below correct? or should it be the name of the module '_' below?
      IntMap.unionWith (<>) ops (IntMap.map (\xs -> [(fix, Scope modNm, op) | (fix, _, op) <- xs]) ops)
    Nothing -> mempty

moduleOpsTables :: Prelude m c -> Map.Map ModuleName OpsTable
moduleOpsTables Prelude {moduleMap} = Map.map (\Module {moduleOpsTable} -> moduleOpsTable) moduleMap

-- | Map from Module.name to the pinned hash for all names in the given prelude.
-- This functions includes the Inferno.Module.Builtin module and also "exports"
-- the Base module so that it can be used without prefix.
preludePinMap :: Prelude m c -> Map.Map (Scoped ModuleName) (Map.Map Namespace (Pinned VCObjectHash))
preludePinMap prelude =
  Pinned.openModule "Base" $
    Pinned.insertBuiltinModule $
      Map.foldrWithKey Pinned.insertHardcodedModule mempty $
        Map.map (Map.map Builtin . pinnedModuleNameToHash) $
          moduleMap prelude

preludeTermEnv :: Prelude m c -> TermEnv VCObjectHash c (ImplEnvM m c) ()
preludeTermEnv = combineTermEnvs . moduleMap

preludeNameToTypeMap :: Prelude m c -> Map.Map (Maybe ModuleName, Namespace) (TypeMetadata TCScheme)
preludeNameToTypeMap prelude =
  let unqualifiedN2h = pinnedModuleNameToHash $ modules Map.! "Base"
      n2h =
        Map.unions $
          Map.mapKeys (Nothing,) (pinnedModuleNameToHash builtinModule)
            : Map.mapKeys (Nothing,) unqualifiedN2h
            : [Map.mapKeys (Just nm,) (pinnedModuleNameToHash m `Map.difference` unqualifiedN2h) | (nm, m) <- Map.toList modules]
      h2ty = Map.unions $ pinnedModuleHashToTy builtinModule : [pinnedModuleHashToTy m | m <- Map.elems modules]
   in Map.mapMaybe (`Map.lookup` h2ty) n2h
  where
    modules = moduleMap prelude

combineTermEnvs ::
  Map.Map ModuleName (PinnedModule (TermEnv VCObjectHash c (ImplEnvM m c) a)) ->
  TermEnv VCObjectHash c (ImplEnvM m c) a
combineTermEnvs modules = foldM (\env m -> (env <>) <$> pinnedModuleTerms m) mempty $ Map.elems modules

-- | A specialiazation of @buildPinnedQQModules@ below with an empty initial prelude.
-- This is to be used in the QuasiQuoter to build the initial/core Inferno prelude.
-- We can't use @buildPinnedQQModules emptyPrelude@ in the QuasiQuoter because TH
-- doesn't like that.
buildInitPrelude ::
  (MonadThrow m, Pretty c) =>
  [(ModuleName, OpsTable, [TopLevelDefn (Either (TCScheme, Value c (ImplEnvM m c)) (Maybe TCScheme, Expr () SourcePos))])] ->
  Prelude m c
buildInitPrelude = buildPinnedQQModules emptyPrelude

buildPinnedQQModules ::
  (MonadThrow m, Pretty c) =>
  Prelude m c ->
  [(ModuleName, OpsTable, [TopLevelDefn (Either (TCScheme, Value c (ImplEnvM m c)) (Maybe TCScheme, Expr () SourcePos))])] ->
  Prelude m c
buildPinnedQQModules initPrelude modules =
  foldl'
    ( \Prelude {pinnedModuleMap, moduleMap} (moduleNm, opsTable, sigs) ->
        -- first build the new module
        let newMod =
              buildModule pinnedModuleMap moduleMap sigs $
                Module
                  { moduleName = moduleNm,
                    moduleOpsTable = opsTable,
                    moduleTypeClasses = mempty,
                    moduleObjects = (Map.singleton (ModuleNamespace moduleNm) $ vcHash $ BuiltinModuleHash moduleNm, mempty, pure mempty)
                  }
         in -- then insert it into the temporary module pin map as well as the final module map
            Prelude
              { pinnedModuleMap = Pinned.insertHardcodedModule moduleNm (Map.map Builtin $ pinnedModuleNameToHash newMod) pinnedModuleMap,
                moduleMap = Map.insert moduleNm newMod moduleMap
              }
    )
    initPrelude
    modules
  where
    buildModule ::
      (MonadThrow m, Pretty c) =>
      Map.Map (Scoped ModuleName) (Map.Map Namespace (Pinned VCObjectHash)) ->
      Map.Map ModuleName (PinnedModule (TermEnv VCObjectHash c (ImplEnvM m c) ())) ->
      [TopLevelDefn (Either (TCScheme, Value c (ImplEnvM m c)) (Maybe TCScheme, Expr () SourcePos))] ->
      PinnedModule (TermEnv VCObjectHash c (ImplEnvM m c) ()) ->
      PinnedModule (TermEnv VCObjectHash c (ImplEnvM m c) ())
    buildModule _ _ [] m = m
    buildModule alreadyPinnedModulesMap alreadyBuiltModules (Signature {..} : xs) m@Module {moduleName, moduleObjects = (nsMap, tyMap, mTrmEnv)} =
      let sigVarToNamespace = \case
            SigVar n -> FunNamespace $ Ident n
            SigOpVar n -> OpNamespace $ Ident n
          (sig, ns, hsh, mTrmEnv') = case def of
            Left (sig', val) ->
              let ns' = sigVarToNamespace name
                  hsh' = vcHash $ BuiltinFunHash (sigVarToExpr LocalScope name, sig)
               in (sig', ns', hsh', (\(local, pinned) -> (local, Map.insert hsh (Right val) pinned)) mTrmEnv)
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
                      (sig', ns', hsh', (\(local, pinned) -> (local, Map.insert hsh (Left finalExpr) pinned)) mTrmEnv)
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
