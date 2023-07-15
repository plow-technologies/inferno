{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Inferno.ML.Module.Prelude (mlPrelude) where

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
import Inferno.Types.Module (PinnedModule)
import Inferno.Types.Syntax
  ( Ident,
    ModuleName (..),
  )
import Inferno.Types.Value (ImplEnvM, Value (..))
import Inferno.Types.VersionControl (VCObjectHash)
import Torch
import qualified Torch.DType as TD
import Torch.Functional
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
        shp <- fromValue vShape
        toValue $ zeros shp opts
    _ -> throwM $ RuntimeError "zerosFun: expecting a dtype enum"

onesFun :: (MonadThrow m) => Value MlValue m
onesFun =
  VFun $ \case
    VEnum _ e -> do
      opts <- getDtype "ones" e
      return $ VFun $ \vShape -> do
        shp <- fromValue vShape
        toValue $ ones shp opts
    _ -> throwM $ RuntimeError "onesFun: expecting a dtype enum"

asTensor1Fun :: (MonadThrow m) => Value MlValue m
asTensor1Fun =
  VFun $ \case
    VArray xs -> do
      fs <- getDoubleList xs
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

asScalar :: Tensor -> Double
asScalar t = asValue $ toType TD.Double t

asArray1Fun :: Tensor -> [Double]
asArray1Fun t = asValue $ toType TD.Double t

asArray2Fun :: Tensor -> [[Double]]
asArray2Fun t = asValue $ toType TD.Double t

argmaxFun :: Int -> Bool -> Tensor -> Tensor
argmaxFun i keepDim t = argmax (Dim i) (if keepDim then KeepDim else RemoveDim) t

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
  unIV $ forward m (map IVTensor ts)
  where
    unIV = \case
      IVTensor t' -> [t']
      IVTensorList ts' -> ts'
      IVTuple ivs ->
        concat $ map unIV ivs
      res -> error $ "expected tensor result, got " ++ (show res)

randnIOFun :: (MonadThrow m, MonadIO m) => Value MlValue m
randnIOFun =
  VFun $ \case
    VEnum _ e -> do
      opts <- getDtype "randnIO" e
      pure $ VFun $ \xs -> do
        shp <- fromValue xs
        t <- liftIO $ randnIO shp opts
        pure $ VCustom $ VTensor t
    _ -> throwM $ RuntimeError "randnIOFun: expecting a dtype enum"

toDeviceFun :: Text -> Tensor -> Tensor
toDeviceFun d t =
  let dev = case d of
        "cpu" -> Device CPU 0
        "cuda:0" -> Device CUDA 0
        device' -> error $ "Unknown device setting: " ++ unpack device'
   in toDevice dev t

mlModules :: (MonadThrow m, MonadIO m) => Prelude.ModuleMap m MlValue
mlModules =
  [mlQuoter|

module ML

  enum dtype := #int | #float | #double;

  zeros : dtype{#int, #float, #double} -> array of int -> tensor := ###!zerosFun###;

  ones : dtype{#int, #float, #double} -> array of int -> tensor := ###!onesFun###;

  add : tensor -> tensor -> tensor := ###add###;

  asTensor1 : array of double -> tensor := ###!asTensor1Fun###;

  asTensor2 : array of (array of double) -> tensor := ###!asTensor2Fun###;

  asTensor4 : array of (array of (array of (array of double))) -> tensor := ###!asTensor4Fun###;

  asScalar : tensor -> double := ###asScalar###;

  asArray1 : tensor -> array of double := ###asArray1Fun###;

  asArray2 : tensor -> array of (array of double) := ###asArray2Fun###;

  sumAll : tensor -> tensor := ###sumAll###;

  powT : int -> tensor -> tensor := ###powTFun###;

  tanH : tensor -> tensor := ###tanHTFun###;

  @doc `argmax i k t` is the argmax of tensor `t` along dimension `i`. `k` denotes whether the output tensor has dim retained or not.;
  argmax : int -> bool{#true, #false} -> tensor -> tensor := ###argmaxFun###;

  softmax : int -> tensor -> tensor := ###softmaxFun###;

  transpose2D : tensor -> tensor := ###transpose2D###;

  matmul : tensor -> tensor -> tensor := ###matmul###;

  mseLoss : tensor -> tensor -> tensor := ###mseLoss###;

  @doc An impure (pseudo)random tensor generator;
  randnIO : dtype{#int, #float, #double} -> array of int -> tensor := ###!randnIOFun###;

  @doc Move a tensor to a different device, e.g. "cpu" or "cuda:0";
  toDevice : text -> tensor -> tensor := ###toDeviceFun###;

  loadModel : text -> model := ###loadModelFun###;

  forward : model -> array of tensor -> array of tensor := ###forwardFun###;

|]

mlPrelude :: Map.Map ModuleName (PinnedModule (ImplEnvM IO MlValue (Eval.TermEnv VCObjectHash MlValue (ImplEnvM IO MlValue))))
mlPrelude =
  Map.unionWith
    (error "Duplicate module name in builtinModules")
    (Prelude.builtinModules @IO @MlValue)
    (mlModules @IO)
