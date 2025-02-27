{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wwarn #-}

module Inferno.ML.Module.Prelude (mlPrelude) where

import Control.Monad.Catch
  ( Exception (displayException),
    MonadCatch,
    MonadThrow (throwM),
    SomeException,
    try,
  )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, unpack)
import GHC.IO.Unsafe (unsafePerformIO)
import Inferno.Eval.Error (EvalError (..))
import Inferno.ML.Types.Value
import Inferno.Module.Cast (FromValue (fromValue), ToValue (toValue))
import qualified Inferno.Module.Prelude as Prelude
import Inferno.Types.Syntax (Ident)
import Inferno.Types.Value (Value (..))
import Prettyprinter (Pretty)
import Torch
import qualified Torch.DType as TD
import Torch.Functional
import qualified Torch.Script as TS

getDtype :: (MonadThrow m) => String -> Ident -> m DType
getDtype funName = \case
  "int" -> return TD.Int64
  "float" -> return TD.Float
  "double" -> return TD.Double
  s -> throwM $ RuntimeError $ funName ++ ": unknown dtype " ++ show s

-- Get the Torch device from a `device{#cpu, #cuda}`. There will only ever
-- be one CUDA device available for our purposes, i.e. `cuda:0`, so we only
-- have to distinguish between CPU and CUDA
getDevice :: (MonadThrow m) => Ident -> m Device
getDevice = \case
  "cpu" -> pure $ Device CPU 0
  "cuda" -> pure $ Device CUDA 0
  s ->
    throwM . RuntimeError $
      unwords
        [ "toDevice :"
        , "unknown device"
        , show s <> ";"
        , "expected one of {#cpu,#cuda}"
        ]

zerosFun :: (MonadThrow m, Pretty a) => Value (MlValue a) m
zerosFun =
  VFun $ \case
    VEnum _ e -> do
      dType <- getDtype "zeros" e
      return $ VFun $ \vShape -> do
        shp <- fromValue vShape
        pure $ toValue $ zeros shp $ withDType dType defaultOpts
    _ -> throwM $ RuntimeError "zerosFun: expecting a dtype enum"

onesFun :: (MonadThrow m, Pretty x) => Value (MlValue x) m
onesFun =
  VFun $ \case
    VEnum _ e -> do
      dType <- getDtype "ones" e
      return $ VFun $ \vShape -> do
        shp <- fromValue vShape
        pure $ toValue $ ones shp $ withDType dType defaultOpts
    _ -> throwM $ RuntimeError "onesFun: expecting a dtype enum"

asTensorFun ::
  forall a x m.
  ( TensorLike a
  , FromValue
      (MlValue x)
      m
      a
  , MonadThrow m
  ) =>
  String ->
  Proxy a ->
  Value (MlValue x) m
asTensorFun funName _proxy =
  VFun $ \case
    VEnum _ e -> do
      dType <- getDtype funName e
      pure $ VFun $ \v -> do
        xs :: a <- fromValue v
        pure $ VCustom $ VTensor $ toType dType $ asTensor xs
    _ -> throwM $ RuntimeError $ funName ++ ": expecting a dtype enum"

toTypeFun :: forall m x. (Pretty x, MonadThrow m) => Value (MlValue x) m
toTypeFun =
  VFun $ \case
    VEnum _ e ->
      getDtype "toType" e <&> \dt ->
        VFun $ fmap (VCustom . VTensor . toType dt) . fromValue
    _ -> throwM . RuntimeError $ "toType : expecting a dtype enum"

asTensor0Fun :: forall m x. (MonadThrow m, Pretty x) => Value (MlValue x) m
asTensor0Fun = asTensorFun "asTensor0" (Proxy :: Proxy Double)

asTensor1Fun :: forall m x. (MonadThrow m, Pretty x) => Value (MlValue x) m
asTensor1Fun = asTensorFun "asTensor1" (Proxy :: Proxy [Double])

asTensor2Fun :: forall m x. (MonadThrow m, Pretty x) => Value (MlValue x) m
asTensor2Fun = asTensorFun "asTensor2" (Proxy :: Proxy [[Double]])

asTensor3Fun :: forall m x. (MonadThrow m, Pretty x) => Value (MlValue x) m
asTensor3Fun = asTensorFun "asTensor3" (Proxy :: Proxy [[[Double]]])

asTensor4Fun :: forall m x. (MonadThrow m, Pretty x) => Value (MlValue x) m
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
argmaxFun i keepDim = argmax (Dim i) (if keepDim then KeepDim else RemoveDim)

softmaxFun :: Int -> Tensor -> Tensor
softmaxFun i = softmax (Dim i)

stackFun :: Int -> [Tensor] -> Tensor
stackFun i = Torch.stack (Dim i)

tanHTFun :: Tensor -> Tensor
tanHTFun = Torch.Functional.tanh

powTFun :: Int -> Tensor -> Tensor
powTFun = pow

unsafeLoadScriptFun :: Text -> ScriptModule
unsafeLoadScriptFun f = unsafePerformIO $ TS.loadScript TS.WithoutRequiredGrad $ unpack f

loadModelFun ::
  forall m x. (Pretty x, MonadIO m, MonadThrow m) => Value (MlValue x) m
loadModelFun = VFun $ \case
  VCustom (VModelName (ModelName mn)) ->
    either (throwM . RuntimeError . displayException) (pure . VCustom . VModel)
      =<< liftIO (try @_ @SomeException loadModel)
    where
      loadModel :: IO ScriptModule
      loadModel = TS.loadScript TS.WithoutRequiredGrad mn
  _ -> throwM $ RuntimeError "Expected a modelName"

forwardFun :: ScriptModule -> [Tensor] -> [Tensor]
forwardFun m ts =
  unIV $ forward m (map IVTensor ts)
  where
    unIV = \case
      IVTensor t' -> [t']
      IVTensorList ts' -> ts'
      IVTuple ivs -> concatMap unIV ivs
      res -> error $ "expected tensor result, got " ++ show res

randnIOFun ::
  forall m x.
  ( MonadThrow m
  , MonadIO m
  , Pretty x
  ) =>
  Value (MlValue x) m
randnIOFun =
  VFun $ \case
    VEnum _ e -> do
      dType <- getDtype "randnIO" e
      pure $ VFun $ \xs -> do
        shp <- fromValue xs
        t <- liftIO $ randnIO shp $ withDType dType defaultOpts
        pure $ VCustom $ VTensor t
    _ -> throwM $ RuntimeError "randnIOFun: expecting a dtype enum"

toDeviceUnsafeFun :: Text -> Tensor -> Tensor
toDeviceUnsafeFun d t =
  let dev = case d of
        "cpu" -> Device CPU 0
        "cuda:0" -> Device CUDA 0
        device' -> error $ "Unknown device setting: " ++ unpack device'
   in toDevice dev t

toDeviceFun ::
  forall m x.
  ( MonadThrow m
  , MonadIO m
  , Pretty x
  ) =>
  Value (MlValue x) m
toDeviceFun =
  VFun $ \case
    VEnum _ e ->
      getDevice e <&> \dev ->
        VFun $ \tensor ->
          (toValue @_ @_ @Tensor) . toDevice dev
            <$> (fromValue @_ @_ @Tensor) tensor
    _ -> throwM $ RuntimeError "toDeviceFun: expecting a device enum"

mlModules ::
  forall m x.
  ( MonadThrow m
  , MonadIO m
  , Pretty x
  ) =>
  Prelude.ModuleMap m (MlValue x)
mlModules =
  [mlQuoter|

module ML

  enum dtype := #int | #float | #double;

  enum device := #cpu | #cuda;

  zeros : dtype{#int, #float, #double} -> array of int -> tensor := ###!zerosFun###;

  ones : dtype{#int, #float, #double} -> array of int -> tensor := ###!onesFun###;

  add : tensor -> tensor -> tensor := ###add###;

  toType : dtype{#int, #float, #double} -> tensor -> tensor := ###!toTypeFun###;

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

  @doc Move a tensor to a different device;
  toDevice : device{#cpu, #cuda} -> tensor -> tensor := ###!toDeviceFun###;

  @doc Move a tensor to a different device, e.g. "cpu" or "cuda:0" (without checking validity of device name);
  toDeviceUnsafe : text -> tensor -> tensor := ###toDeviceUnsafeFun###;

  @doc Load a named, serialized model;
  loadModel : modelName -> model := ###!loadModelFun###;

  unsafeLoadScript : text -> model := ###unsafeLoadScriptFun###;

  forward : model -> array of tensor -> array of tensor := ###forwardFun###;

|]

mlPrelude ::
  forall m x.
  ( MonadIO m
  , MonadCatch m
  , Pretty x
  , Eq x
  ) =>
  Prelude.ModuleMap m (MlValue x)
mlPrelude =
  Map.unionWith
    (error "Duplicate module name in builtinModules")
    (Prelude.builtinModules @m @(MlValue x))
    (mlModules @m)
