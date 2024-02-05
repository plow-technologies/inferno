{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Inferno.ML.Module.Prelude (mlPrelude) where

import Control.Monad.Catch (MonadCatch, MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, unpack)
import GHC.IO.Unsafe (unsafePerformIO)
import Inferno.Eval.Error (EvalError (..))
import Inferno.ML.Module.QQ (preludeQuoter)
import Inferno.ML.Types.Value
import Inferno.Module (Prelude)
import Inferno.Module.Cast (FromValue (fromValue), ToValue (toValue))
import Inferno.Types.Syntax (Ident)
import Inferno.Types.Value (Value (..))
import Torch
import qualified Torch.DType as TD
import Torch.Functional
import qualified Torch.Script as TS

getDtype :: (MonadThrow m) => String -> Ident -> m DType
getDtype funName = \case
  "int" -> return TD.Int64
  "float" -> return TD.Float
  "double" -> return TD.Double
  s -> throwM $ RuntimeError $ funName ++ ": unknown dtype " ++ (show s)

zerosFun :: (MonadThrow m) => Value MlValue m
zerosFun =
  VFun $ \case
    VEnum _ e -> do
      dType <- getDtype "zeros" e
      return $ VFun $ \vShape -> do
        shp <- fromValue vShape
        pure $ toValue $ zeros shp $ withDType dType defaultOpts
    _ -> throwM $ RuntimeError "zerosFun: expecting a dtype enum"

onesFun :: (MonadThrow m) => Value MlValue m
onesFun =
  VFun $ \case
    VEnum _ e -> do
      dType <- getDtype "ones" e
      return $ VFun $ \vShape -> do
        shp <- fromValue vShape
        pure $ toValue $ ones shp $ withDType dType defaultOpts
    _ -> throwM $ RuntimeError "onesFun: expecting a dtype enum"

asTensorFun :: forall a m. (TensorLike a, FromValue MlValue m a, MonadThrow m) => String -> Proxy a -> Value MlValue m
asTensorFun funName _proxy =
  VFun $ \case
    VEnum _ e -> do
      dType <- getDtype funName e
      pure $ VFun $ \v -> do
        xs :: a <- fromValue v
        pure $ VCustom $ VTensor $ toType dType $ asTensor $ xs
    _ -> throwM $ RuntimeError $ funName ++ ": expecting a dtype enum"

asTensor0Fun :: (MonadThrow m) => Value MlValue m
asTensor0Fun = asTensorFun "asTensor0" (Proxy :: Proxy Double)

asTensor1Fun :: (MonadThrow m) => Value MlValue m
asTensor1Fun = asTensorFun "asTensor1" (Proxy :: Proxy [Double])

asTensor2Fun :: (MonadThrow m) => Value MlValue m
asTensor2Fun = asTensorFun "asTensor2" (Proxy :: Proxy [[Double]])

asTensor3Fun :: (MonadThrow m) => Value MlValue m
asTensor3Fun = asTensorFun "asTensor3" (Proxy :: Proxy [[[Double]]])

asTensor4Fun :: (MonadThrow m) => Value MlValue m
asTensor4Fun = asTensorFun "asTensor4" (Proxy :: Proxy [[[[Double]]]])

asDouble :: Tensor -> Double
asDouble t = asValue $ toType TD.Double t

asArray1Fun :: Tensor -> [Double]
asArray1Fun t = asValue $ toType TD.Double t

asArray2Fun :: Tensor -> [[Double]]
asArray2Fun t = asValue $ toType TD.Double t

asArray3Fun :: Tensor -> [[[Double]]]
asArray3Fun t = asValue $ toType TD.Double t

asArray4Fun :: Tensor -> [[[[Double]]]]
asArray4Fun t = asValue $ toType TD.Double t

argmaxFun :: Int -> Bool -> Tensor -> Tensor
argmaxFun i keepDim t = argmax (Dim i) (if keepDim then KeepDim else RemoveDim) t

softmaxFun :: Int -> Tensor -> Tensor
softmaxFun i t = softmax (Dim i) t

stackFun :: Int -> [Tensor] -> Tensor
stackFun i t = Torch.stack (Dim i) t

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
      dType <- getDtype "randnIO" e
      pure $ VFun $ \xs -> do
        shp <- fromValue xs
        t <- liftIO $ randnIO shp $ withDType dType defaultOpts
        pure $ VCustom $ VTensor t
    _ -> throwM $ RuntimeError "randnIOFun: expecting a dtype enum"

toDeviceFun :: Text -> Tensor -> Tensor
toDeviceFun d t =
  let dev = case d of
        "cpu" -> Device CPU 0
        "cuda:0" -> Device CUDA 0
        device' -> error $ "Unknown device setting: " ++ unpack device'
   in toDevice dev t

mlPrelude :: (MonadThrow m, MonadCatch m, MonadIO m) => Prelude m MlValue
mlPrelude =
  [preludeQuoter|

module ML

  enum dtype := #int | #float | #double;

  zeros : dtype{#int, #float, #double} -> array of int -> tensor := ###!zerosFun###;

  ones : dtype{#int, #float, #double} -> array of int -> tensor := ###!onesFun###;

  add : tensor -> tensor -> tensor := ###add###;

  asTensor0 : dtype{#int, #float, #double} -> double -> tensor := ###!asTensor0Fun###;

  asTensor1 : dtype{#int, #float, #double} -> array of double -> tensor := ###!asTensor1Fun###;

  asTensor2 : dtype{#int, #float, #double} -> array of (array of double) -> tensor := ###!asTensor2Fun###;

  asTensor3 : dtype{#int, #float, #double} -> array of (array of (array of double)) -> tensor := ###!asTensor3Fun###;

  asTensor4 : dtype{#int, #float, #double} -> array of (array of (array of (array of double))) -> tensor := ###!asTensor4Fun###;

  asDouble : tensor -> double := ###asDouble###;

  asArray1 : tensor -> array of double := ###asArray1Fun###;

  asArray2 : tensor -> array of (array of double) := ###asArray2Fun###;

  asArray3 : tensor -> array of (array of (array of double)) := ###asArray3Fun###;

  asArray4 : tensor -> array of (array of (array of (array of double))) := ###asArray4Fun###;

  sumAll : tensor -> tensor := ###sumAll###;

  powT : int -> tensor -> tensor := ###powTFun###;

  tanH : tensor -> tensor := ###tanHTFun###;

  @doc `argmax i k t` is the argmax of tensor `t` along dimension `i`. `k` denotes whether the output tensor has dim retained or not.;
  argmax : int -> bool{#true, #false} -> tensor -> tensor := ###argmaxFun###;

  softmax : int -> tensor -> tensor := ###softmaxFun###;

  @doc `stack i [t]` takes an array of tensors `t` and appends them along the dimension i in a new tensor;
  stack : int -> array of tensor -> tensor := ###stackFun###;

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
