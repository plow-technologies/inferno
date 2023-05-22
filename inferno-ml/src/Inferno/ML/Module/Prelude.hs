{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Inferno.ML.Module.Prelude where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (ExceptT, MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import GHC.IO.Unsafe (unsafePerformIO)
import Inferno.Eval as Eval (runEvalIO, TermEnv)
import Inferno.Eval.Error (EvalError (..))
import Inferno.Module.Cast (ToValue (toValue), FromValue (fromValue))
import qualified Inferno.Module.Prelude as Prelude
import Inferno.Parse (OpsTable)
import Inferno.Types.Module (PinnedModule)
import Inferno.Types.Syntax
  ( Expr (..),
    ExtIdent (..),
    Ident,
    ModuleName (..),
    Scoped (..),
  )
import Inferno.Types.Type (Namespace (..), TCScheme, TypeMetadata)
import Inferno.Types.Value (ImplEnvM, Value (..))
import Inferno.Types.VersionControl (Pinned (..), VCObjectHash)
import Inferno.Utils.QQ.Module (infernoModules)
import Inferno.ML.Types.Value
import qualified Torch as T
import qualified Torch.DType as TD
import qualified Torch.Functional as TF
import qualified Torch.Script as TS

-- TODO redefine addition on tensors as well here somehow

zerosFun :: (MonadError EvalError m) => Value MlValue m
zerosFun =
  VFun $ \case
    VEnum _ e -> do
      opts <- getDtype e
      return $ VFun $ \vShape -> do
        shape <- fromValue vShape
        t <- toValue $ T.zeros shape opts
        return t
    _ -> throwError $ RuntimeError "zerosFun: expecting a dtype enum"
  where
    getDtype :: (MonadError EvalError m) => Ident -> m T.TensorOptions
    getDtype = \case
      "int" -> return $ T.withDType TD.Int64 T.defaultOpts
      "float" -> return $ T.withDType TD.Float T.defaultOpts
      "double" -> return $ T.withDType TD.Double T.defaultOpts
      s -> throwError $ RuntimeError $ "zerosFun: unknown dtype " ++ (show s)

asTensor1Fun :: (MonadError EvalError m) => Value MlValue m
asTensor1Fun =
  VFun $ \case
    VArray xs -> do
      fs <- getDoubleList xs
      -- pure $ VCustom $ VTensor $ T.asTensor $ fs
      pure $ VCustom $ VTensor $ T.toType TD.Float $ T.asTensor $ fs
    _ -> throwError $ RuntimeError "asTensor2Fun: expecting an array"
  where
    getDouble v = case v of
      VDouble x -> pure x
      _ -> throwError $ RuntimeError "asTensor2Fun: expecting double values"
    getDoubleList xs = mapM getDouble xs

asTensor2Fun :: (MonadError EvalError m) => Value MlValue m
asTensor2Fun =
  VFun $ \case
    VArray xs -> do
      fs <- mapM getDoubleList xs
      -- pure $ VCustom $ VTensor $ T.asTensor $ fs
      pure $ VCustom $ VTensor $ T.toType TD.Float $ T.asTensor $ fs
    _ -> throwError $ RuntimeError "asTensor2Fun: expecting an array"
  where
    getDouble v = case v of
      VDouble x -> pure x
      _ -> throwError $ RuntimeError "asTensor2Fun: expecting double values"
    getDoubleList = \case
      VArray xs -> mapM getDouble xs
      _ -> throwError $ RuntimeError "asTensor2Fun: expecting an array of arrays"

-- TODO clean up
asTensor4Fun :: (MonadError EvalError m) => Value MlValue m
asTensor4Fun =
  VFun $ \case
    VArray xs -> do
      fs <- mapM getDoubleListListList xs
      -- pure $ VCustom $ VTensor $ T.asTensor $ fs
      pure $ VCustom $ VTensor $ T.toType TD.Float $ T.asTensor $ fs
    _ -> throwError $ RuntimeError "asTensor4Fun: expecting an array"
  where
    getDouble v = case v of
      VDouble x -> pure x
      _ -> throwError $ RuntimeError "asTensor4Fun: expecting double values"
    getDoubleList = \case
      VArray xs -> mapM getDouble xs
      _ -> throwError $ RuntimeError "asTensor4Fun: expecting an array of arrays"
    getDoubleListList = \case
      VArray xs -> mapM getDoubleList xs
      _ -> throwError $ RuntimeError "asTensor4Fun: expecting an array of arrays"
    getDoubleListListList = \case
      VArray xs -> mapM getDoubleListList xs
      _ -> throwError $ RuntimeError "asTensor4Fun: expecting an array of arrays"

asArray1Fun :: T.Tensor -> [Double]
asArray1Fun t = T.asValue $ T.toType TD.Double t

asArray2Fun :: T.Tensor -> [[Double]]
asArray2Fun t = T.asValue $ T.toType TD.Double t

transpose2DFun :: T.Tensor -> T.Tensor
transpose2DFun t = TF.transpose2D t

matmulFun :: T.Tensor -> T.Tensor -> T.Tensor
matmulFun t1 t2 = T.matmul t1 t2

softmaxFun :: Int -> T.Tensor -> T.Tensor
softmaxFun i t = T.softmax (T.Dim i) t

sumAllFun :: T.Tensor -> T.Tensor
sumAllFun = T.sumAll

tanHTFun :: T.Tensor -> T.Tensor
tanHTFun t = TF.tanh t

loadModelFun :: Text -> T.ScriptModule
loadModelFun f = unsafePerformIO $ TS.loadScript TS.WithoutRequiredGrad $ unpack f

forwardFun :: T.ScriptModule -> [T.Tensor] -> [T.Tensor]
forwardFun m ts =
  -- let mkIVT t = T.IVTensor $ T.toType TD.Float t in
  -- let ivts = T.IVTensorList $ map mkIVT ts in
  unIV $ T.forward m (map T.IVTensor ts)
  where
    unIV = \case
      T.IVTensor t' -> [t']
      T.IVTensorList ts' -> ts'
      T.IVTuple ivs ->
        concat $ map unIV ivs
      res -> error $ "expected tensor result, got " ++ (show res)

powTFun :: Int -> T.Tensor -> T.Tensor
powTFun i t = T.pow i t

randomTensorIFun :: (MonadError EvalError m, MonadIO m) => Value MlValue m
randomTensorIFun = VFun $ \xs -> do
  -- TODO also allow choosing dtype
  -- TODO if this works also use this in toTensor functions above
  size <- fromValue xs
  t <- liftIO $ T.randnIO' size
  -- t <- liftIO $ T.randnIO size (T.withDType TD.Double T.defaultOpts)
  pure $ VCustom $ VTensor t

mlModules :: (MonadError EvalError m, MonadIO m) => Prelude.ModuleMap m MlValue
mlModules =
  [infernoModules|

module ML

  define addition on tensor tensor tensor;

  enum dtype := #int | #float | #double;

  zeros : dtype{#int, #float, #double} -> array of int -> tensor := ###!zerosFun###;

  asTensor1 : array of double -> tensor := ###!asTensor1Fun###;

  asTensor2 : array of (array of double) -> tensor := ###!asTensor2Fun###;

  asTensor4 : array of (array of (array of (array of double))) -> tensor := ###!asTensor4Fun###;

  asArray1 : tensor -> array of double := ###asArray1Fun###;

  asArray2 : tensor -> array of (array of double) := ###asArray2Fun###;

  sumAll : tensor -> tensor := ###sumAllFun###;

  powT : int -> tensor -> tensor := ###powTFun###;

  tanH : tensor -> tensor := ###tanHTFun###;

  softmax : int -> tensor -> tensor := ###softmaxFun###;

  transpose2D : tensor -> tensor := ###transpose2DFun###;

  matmul : tensor -> tensor -> tensor := ###matmulFun###;

  @doc An impure (pseudo)random tensor generator;
  randomTensorI: array of int -> tensor := ###!randomTensorIFun###;

|]

  -- // TODO: these require adding a Model type and value type:
  -- loadModel : text -> model := ###loadModelFun###;

  -- forward : model -> array of tensor -> array of tensor := ###forwardFun###;

builtinModules :: Map.Map ModuleName (PinnedModule (ImplEnvM (ExceptT EvalError IO) MlValue (Eval.TermEnv VCObjectHash MlValue (ImplEnvM (ExceptT EvalError IO) MlValue))))
builtinModules =
  -- -- "Export" the TachDB module so that its functions can be used without a module prefix
  -- case Map.lookup "Base" modules of
  --   Nothing -> error "plow-inferno builtinModules: module Base not found"
  --   Just Module {moduleName, moduleOpsTable = opsTable, moduleTypeClasses = tyCls, moduleObjects = (nsMap, tyMap, mTrmEnv)} ->
  --     case Map.lookup "TachDB" modules of
  --       Nothing -> error "plow-inferno builtinModules: module TachDB not found"
  --       Just Module {moduleOpsTable = opsTable', moduleTypeClasses = tyCls', moduleObjects = (nsMap', tyMap', mTrmEnv')} ->
  --         Map.insert
  --           "Base"
  --           Module
  --             { moduleName,
  --               moduleOpsTable = IntMap.unionWith (<>) opsTable opsTable',
  --               moduleTypeClasses = tyCls <> tyCls',
  --               moduleObjects = (nsMap <> nsMap', tyMap <> tyMap', mTrmEnv >>= \x -> mTrmEnv' >>= \y -> pure $ x <> y)
  --             }
  --           (Map.delete "TachDB" modules)
  -- where
  --   modules =
  --     Map.unionWith
  --       (error "Duplicate module name in builtinModules")
  --       (Prelude.builtinModules @(ExceptT EvalError IO) @PlowValue)
  --       (plowModules @(ExceptT EvalError IO))
  Map.unionWith
    (error "Duplicate module name in builtinModules")
    (Prelude.builtinModules @(ExceptT EvalError IO) @MlValue)
    (mlModules @(ExceptT EvalError IO))

baseOpsTable :: OpsTable
baseOpsTable = Prelude.baseOpsTable @(ExceptT EvalError IO) @MlValue builtinModules

builtinModulesOpsTable :: Map.Map ModuleName OpsTable
builtinModulesOpsTable = Prelude.builtinModulesOpsTable @(ExceptT EvalError IO) @MlValue builtinModules

builtinModulesPinMap :: Map.Map (Scoped ModuleName) (Map.Map Namespace (Pinned VCObjectHash))
builtinModulesPinMap = Prelude.builtinModulesPinMap @(ExceptT EvalError IO) @MlValue builtinModules

builtinModulesTerms :: ImplEnvM (ExceptT EvalError IO) MlValue (Eval.TermEnv VCObjectHash MlValue (ImplEnvM (ExceptT EvalError IO) MlValue))
builtinModulesTerms = Prelude.builtinModulesTerms @(ExceptT EvalError IO) @MlValue builtinModules

preludeNameToTypeMap :: Map.Map (Maybe ModuleName, Namespace) (TypeMetadata TCScheme)
preludeNameToTypeMap = Prelude.preludeNameToTypeMap @(ExceptT EvalError IO) @MlValue builtinModules

runEvalIO ::
  (MonadCatch m) =>
  ImplEnvM (ExceptT EvalError m) MlValue (Eval.TermEnv VCObjectHash MlValue (ImplEnvM (ExceptT EvalError m) MlValue)) ->
  Map.Map ExtIdent (Value MlValue (ImplEnvM (ExceptT EvalError m) MlValue)) ->
  Expr (Maybe VCObjectHash) a ->
  m (Either EvalError (Value MlValue (ImplEnvM (ExceptT EvalError m) MlValue)))
runEvalIO = Eval.runEvalIO @_ @MlValue
