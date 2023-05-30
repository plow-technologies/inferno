{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Inferno.ML.Module.Prelude (baseOpsTable, builtinModules, builtinModulesOpsTable, builtinModulesPinMap, builtinModulesTerms) where

import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import GHC.IO.Unsafe (unsafePerformIO)
import Inferno.Eval as Eval (TermEnv)
import Inferno.Eval.Error (EvalError (..))
import Inferno.ML.Types.Value
import Inferno.Module.Cast (FromValue (fromValue), ToValue (toValue))
import qualified Inferno.Module.Prelude as Prelude
import Inferno.Parse (OpsTable)
import Inferno.Types.Module (PinnedModule)
import Inferno.Types.Syntax
  ( Ident,
    ModuleName (..),
    Scoped (..),
  )
import Inferno.Types.Type (Namespace (..))
import Inferno.Types.Value (ImplEnvM, Value (..))
import Inferno.Types.VersionControl (Pinned (..), VCObjectHash)
import Inferno.Utils.QQ.Module (infernoModules)
import Torch
  ( Device (..),
    DeviceType (..),
    HasForward (forward),
    IValue (..),
    ScriptModule,
    Tensor,
    TensorLike (asTensor),
    TensorOptions,
    asValue,
    defaultOpts,
    ones,
    randnIO',
    toDevice,
    toType,
    withDType,
    zeros,
  )
import qualified Torch.DType as TD
import Torch.Functional
  ( Dim (Dim),
    add,
    matmul,
    pow,
    softmax,
    sumAll,
    tanh,
    transpose2D,
  )
import qualified Torch.Script as TS

getDtype :: (MonadThrow m) => String -> Ident -> m TensorOptions
getDtype funName = \case
  "int" -> return $ withDType TD.Int64 defaultOpts
  "float" -> return $ withDType TD.Float defaultOpts
  "double" -> return $ withDType TD.Double defaultOpts
  s -> throwM $ RuntimeError $ funName ++ ": unknown dtype " ++ (show s)

zerosFun :: (MonadThrow m) => Value MlValue m
zerosFun =
  VFun $ \case
    VEnum _ e -> do
      opts <- getDtype "zeros" e
      return $ VFun $ \vShape -> do
        shape <- fromValue vShape
        t <- toValue $ zeros shape opts
        return t
    _ -> throwM $ RuntimeError "zerosFun: expecting a dtype enum"

onesFun :: (MonadThrow m) => Value MlValue m
onesFun =
  VFun $ \case
    VEnum _ e -> do
      opts <- getDtype "ones" e
      return $ VFun $ \vShape -> do
        shape <- fromValue vShape
        t <- toValue $ ones shape opts
        return t
    _ -> throwM $ RuntimeError "onesFun: expecting a dtype enum"

asTensor1Fun :: (MonadThrow m) => Value MlValue m
asTensor1Fun =
  VFun $ \case
    VArray xs -> do
      fs <- getDoubleList xs
      -- pure $ VCustom $ VTensor $ asTensor $ fs
      pure $ VCustom $ VTensor $ toType TD.Float $ asTensor $ fs
    _ -> throwM $ RuntimeError "asTensor2Fun: expecting an array"
  where
    getDouble v = case v of
      VDouble x -> pure x
      _ -> throwM $ RuntimeError "asTensor2Fun: expecting double values"
    getDoubleList xs = mapM getDouble xs

asTensor2Fun :: (MonadThrow m) => Value MlValue m
asTensor2Fun =
  VFun $ \case
    VArray xs -> do
      fs <- mapM getDoubleList xs
      -- pure $ VCustom $ VTensor $ asTensor $ fs
      pure $ VCustom $ VTensor $ toType TD.Float $ asTensor $ fs
    _ -> throwM $ RuntimeError "asTensor2Fun: expecting an array"
  where
    getDouble v = case v of
      VDouble x -> pure x
      _ -> throwM $ RuntimeError "asTensor2Fun: expecting double values"
    getDoubleList = \case
      VArray xs -> mapM getDouble xs
      _ -> throwM $ RuntimeError "asTensor2Fun: expecting an array of arrays"

-- TODO clean up
asTensor4Fun :: (MonadThrow m) => Value MlValue m
asTensor4Fun =
  VFun $ \case
    VArray xs -> do
      fs <- mapM getDoubleListListList xs
      -- pure $ VCustom $ VTensor $ asTensor $ fs
      pure $ VCustom $ VTensor $ toType TD.Float $ asTensor $ fs
    _ -> throwM $ RuntimeError "asTensor4Fun: expecting an array"
  where
    getDouble v = case v of
      VDouble x -> pure x
      _ -> throwM $ RuntimeError "asTensor4Fun: expecting double values"
    getDoubleList = \case
      VArray xs -> mapM getDouble xs
      _ -> throwM $ RuntimeError "asTensor4Fun: expecting an array of arrays"
    getDoubleListList = \case
      VArray xs -> mapM getDoubleList xs
      _ -> throwM $ RuntimeError "asTensor4Fun: expecting an array of arrays"
    getDoubleListListList = \case
      VArray xs -> mapM getDoubleListList xs
      _ -> throwM $ RuntimeError "asTensor4Fun: expecting an array of arrays"

asArray1Fun :: Tensor -> [Double]
asArray1Fun t = asValue $ toType TD.Double t

asArray2Fun :: Tensor -> [[Double]]
asArray2Fun t = asValue $ toType TD.Double t

softmaxFun :: Int -> Tensor -> Tensor
softmaxFun i t = softmax (Dim i) t

tanHTFun :: Tensor -> Tensor
tanHTFun = Torch.Functional.tanh

powTFun :: Int -> Tensor -> Tensor
powTFun i t = pow i t

loadModelFun :: Text -> ScriptModule
loadModelFun f = unsafePerformIO $ TS.loadScript TS.WithoutRequiredGrad $ unpack f

forwardFun :: ScriptModule -> [Tensor] -> [Tensor]
forwardFun m ts =
  -- let mkIVT t = IVTensor $ toType TD.Float t in
  -- let ivts = IVTensorList $ map mkIVT ts in
  unIV $ forward m (map IVTensor ts)
  where
    unIV = \case
      IVTensor t' -> [t']
      IVTensorList ts' -> ts'
      IVTuple ivs ->
        concat $ map unIV ivs
      res -> error $ "expected tensor result, got " ++ (show res)

randomTensorIFun :: (MonadThrow m, MonadIO m) => Value MlValue m
randomTensorIFun = VFun $ \xs -> do
  -- TODO also allow choosing dtype
  -- TODO if this works also use this in toTensor functions above
  size <- fromValue xs
  t <- liftIO $ randnIO' size
  -- t <- liftIO $ randnIO size (withDType TD.Double defaultOpts)
  pure $ VCustom $ VTensor t

toDeviceFun :: Text -> Tensor -> Tensor
toDeviceFun d t =
  let device = case d of
        "cpu" -> Device CPU 0
        "cuda:0" -> Device CUDA 0
        device' -> error $ "Unknown device setting: " ++ unpack device'
   in toDevice device t

mlModules :: (MonadThrow m, MonadIO m) => Prelude.ModuleMap m MlValue
mlModules =
  [infernoModules|

module ML

  enum dtype := #int | #float | #double;

  zeros : dtype{#int, #float, #double} -> array of int -> tensor := ###!zerosFun###;

  ones : dtype{#int, #float, #double} -> array of int -> tensor := ###!onesFun###;

  add : tensor -> tensor -> tensor := ###add###;

  asTensor1 : array of double -> tensor := ###!asTensor1Fun###;

  asTensor2 : array of (array of double) -> tensor := ###!asTensor2Fun###;

  asTensor4 : array of (array of (array of (array of double))) -> tensor := ###!asTensor4Fun###;

  asArray1 : tensor -> array of double := ###asArray1Fun###;

  asArray2 : tensor -> array of (array of double) := ###asArray2Fun###;

  sumAll : tensor -> tensor := ###sumAll###;

  powT : int -> tensor -> tensor := ###powTFun###;

  tanH : tensor -> tensor := ###tanHTFun###;

  softmax : int -> tensor -> tensor := ###softmaxFun###;

  transpose2D : tensor -> tensor := ###transpose2D###;

  matmul : tensor -> tensor -> tensor := ###matmul###;

  @doc An impure (pseudo)random tensor generator;
  randomTensorI : array of int -> tensor := ###!randomTensorIFun###;

  @doc Move a tensor to a different device, e.g. "cpu" or "cuda:0";
  toDevice : text -> tensor -> tensor := ###toDeviceFun###;

  loadModel : text -> model := ###loadModelFun###;

  forward : model -> array of tensor -> array of tensor := ###forwardFun###;

|]

builtinModules :: Map.Map ModuleName (PinnedModule (ImplEnvM IO MlValue (Eval.TermEnv VCObjectHash MlValue (ImplEnvM IO MlValue))))
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
  --       (Prelude.builtinModules @IO @PlowValue)
  --       (plowModules @IO)
  Map.unionWith
    (error "Duplicate module name in builtinModules")
    (Prelude.builtinModules @IO @MlValue)
    (mlModules @IO)

baseOpsTable :: OpsTable
baseOpsTable = Prelude.baseOpsTable @IO @MlValue builtinModules

builtinModulesOpsTable :: Map.Map ModuleName OpsTable
builtinModulesOpsTable = Prelude.builtinModulesOpsTable @IO @MlValue builtinModules

builtinModulesPinMap :: Map.Map (Scoped ModuleName) (Map.Map Namespace (Pinned VCObjectHash))
builtinModulesPinMap = Prelude.builtinModulesPinMap @IO @MlValue builtinModules

builtinModulesTerms :: ImplEnvM IO MlValue (Eval.TermEnv VCObjectHash MlValue (ImplEnvM IO MlValue))
builtinModulesTerms = Prelude.builtinModulesTerms @IO @MlValue builtinModules
