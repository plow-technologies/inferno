{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Inferno.ML.Module.Prelude
  ( MlModule,
    defaultMlPrelude,
    defaultMlModule,
    mkMlPrelude,
    getDevice,
  ) where

import Control.Exception (evaluate)
import Control.Monad ((<=<))
import Control.Monad.Catch
  ( Exception (displayException),
    MonadCatch,
    MonadThrow (throwM),
    SomeException,
    catch,
    try,
  )
import Control.Monad.Extra (concatMapM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (bool)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as Text
import GHC.IO.Unsafe (unsafePerformIO)
import Inferno.Eval.Error (EvalError (RuntimeError))
import Inferno.ML.Module.Compat (MkPropertyFuns (MkPropertyFuns))
import qualified Inferno.ML.Module.Compat as Compat
import Inferno.ML.Types.Value
import Inferno.Module.Cast (Either3, FromValue (fromValue), ToValue (toValue))
import qualified Inferno.Module.Prelude as Prelude
import Inferno.Types.Syntax (Ident)
import Inferno.Types.Value
  ( ImplEnvM,
    Value (VArray, VCustom, VEnum, VFun),
  )
import Language.C.Inline.Cpp.Exception (CppException)
import Prettyprinter (Pretty)
import Torch
  ( DType,
    Device (Device),
    DeviceType (CPU, CUDA),
    Dim (Dim),
    IValue (IVTensor, IVTensorList, IVTuple),
    KeepDim (KeepDim, RemoveDim),
    LoadMode (WithoutRequiredGrad),
    ScriptModule,
    Tensor,
    TensorLike,
  )
import qualified Torch
import qualified Torch.DType as DType
import qualified Torch.Functional
import qualified Torch.Script
import qualified Torch.Tensor as Tensor

-- | A @MkMlModule@ implementation that depends on Hasktorch types
type MlModule m x = Compat.MkMlModule m Tensor ScriptModule ModelName x

-- | The default @ML@ Inferno module generator which depends on Hasktorch.
-- Applying @mkMlModule@ will create the @ML@ module with these primitive
-- implementations
defaultMlModule ::
  forall m x.
  ( MonadIO m
  , MonadCatch m
  , Pretty x
  , Eq x
  ) =>
  MlModule m x
defaultMlModule =
  Compat.MkMlModule
    { models =
        Compat.MkModelFuns
          { loadModel =
              VFun $ \case
                VCustom (VModelName (ModelName mn)) ->
                  either
                    (throwM . RuntimeError . displayException)
                    (pure . VCustom . VModel)
                    =<< liftIO (try @_ @SomeException loadModel)
                  where
                    loadModel :: IO ScriptModule
                    loadModel = Torch.Script.loadScript WithoutRequiredGrad mn
                _ -> throwM $ RuntimeError "Expected a modelName"
          , forward =
              let
                expectedTensors :: EvalError
                expectedTensors = RuntimeError "expected an array of tensors"
               in
                VFun $ \case
                  VCustom (VModel model) -> pure . VFun $ \case
                    VArray mts ->
                      getTensors mts >>= \tensors ->
                        -- Note that we are using `forwardIO` here so any C++ exception can be
                        -- caught, otherwise the operation will fail silently (besides printing
                        -- to stderr)
                        fmap (VArray . fmap (VCustom . VTensor)) . liftIO $
                          forwardIO model tensors `catch` torchHandler
                      where
                        -- Unfortunately we need to unwrap all of the constructors to get the
                        -- tensors inside
                        getTensors :: [Value (MlValue x) m] -> m [Tensor]
                        getTensors = traverse $ \case
                          VCustom (VTensor t) -> pure t
                          _ -> throwM expectedTensors

                        -- Rethrow the C++ exception as a `RuntimeError` so we can at least
                        -- see it in error messages more clearly
                        torchHandler :: CppException -> IO [Tensor]
                        torchHandler =
                          throwM
                            . RuntimeError
                            . ("forward: exception from Torchscript interpreter " <>)
                            . displayException
                    _ -> throwM expectedTensors
                  _ -> throwM $ RuntimeError "expected a model"
          , unsafeLoadScript =
              unsafePerformIO
                . Torch.Script.loadScript WithoutRequiredGrad
                . Text.unpack
          }
    , devices =
        Compat.MkDeviceFuns
          { -- NOTE This should be overridden -- we need `toDevice` to check
            -- that the tensor is moved, but we need a concrete `m` to do that
            -- in. Consumers can replace this implementation by updating the
            -- record field. This is the default, less-ideal implementation
            toDevice =
              -- Implementation of `toDevice` that doesn't check the device that the
              -- moved tensor is on (i.e. may fail silently)
              VFun $ \case
                VEnum _ e ->
                  getDevice e <&> \dev ->
                    VFun $ fmap (toValue . Torch.toDevice @Tensor dev) . fromValue
                _ -> throwM $ RuntimeError "toDeviceFun: expecting a device enum"
          , toDeviceUnsafe = \dev t ->
              flip Torch.toDevice t $ case dev of
                "cpu" -> Device CPU 0
                "cuda:0" -> Device CUDA 0
                d -> error $ unwords ["Unknown device setting:", Text.unpack d]
          }
    , factories =
        Compat.MkFactoryFuns
          { zeros =
              withDType "zeros" $ \dt ->
                VFun $
                  fmap (toValue . (`Torch.zeros` Torch.withDType dt Torch.defaultOpts))
                    . fromValue
          , ones =
              withDType "ones" $ \dt ->
                VFun $
                  fmap (toValue . (`Torch.ones` Torch.withDType dt Torch.defaultOpts))
                    . fromValue
          , randnIO =
              withDType "randnIO" $ \dt ->
                VFun $
                  fmap (VCustom . VTensor)
                    . liftIO
                    . (`Torch.randnIO` Torch.withDType dt Torch.defaultOpts)
                    <=< fromValue
          }
    , conversions =
        Compat.MkConversionFuns
          { toType =
              withDType "toType" $ \dt ->
                VFun $ fmap (VCustom . VTensor . Torch.toType dt) . fromValue
          , asTensor0 = asTensorFun "asTensor0" $ Proxy @Double
          , asTensor1 = asTensorFun "asTensor1" $ Proxy @[Double]
          , asTensor2 = asTensorFun "asTensor2" $ Proxy @[[Double]]
          , asTensor3 = asTensorFun "asTensor3" $ Proxy @[[[Double]]]
          , asTensor4 = asTensorFun "asTensor4" $ Proxy @[[[[Double]]]]
          , asDouble = Torch.asValue . Torch.toType DType.Double
          , asArray1 = Torch.asValue . Torch.toType DType.Double
          , asArray2 = Torch.asValue . Torch.toType DType.Double
          , asArray3 = Torch.asValue . Torch.toType DType.Double
          , asArray4 = Torch.asValue . Torch.toType DType.Double
          }
    , functional =
        Compat.MkFunctionalFuns
          { mean = Torch.Functional.mean
          , std = Torch.Functional.std
          , var = Torch.Functional.var
          , sumAll = Torch.Functional.sumAll
          , abs = Torch.Functional.abs
          , frac = Torch.Functional.frac
          , argmax =
              \i -> Torch.Functional.argmax (Dim i) . bool RemoveDim KeepDim
          , add = Torch.Functional.add
          , mul = Torch.Functional.mul
          , sub = Torch.Functional.sub
          , div = Torch.Functional.div
          , ceil = Torch.Functional.ceil
          , floor = Torch.Functional.floor
          , min = Torch.Functional.min
          , max = Torch.Functional.max
          , median = Torch.Functional.median
          , addScalar = withScalar Torch.Functional.addScalar
          , subScalar = withScalar Torch.Functional.subScalar
          , mulScalar = withScalar Torch.Functional.mulScalar
          , divScalar = withScalar Torch.Functional.divScalar
          , matmul = Torch.Functional.matmul
          , oneHot = Torch.Functional.oneHot
          , erf = Torch.Functional.erf
          , erfc = Torch.Functional.erfc
          , erfinv = Torch.Functional.erfinv
          , lgamma = Torch.Functional.lgamma
          , digamma = Torch.Functional.digamma
          , polygamma = Torch.Functional.polygamma
          , mvlgamma = Torch.Functional.mvlgamma
          , exp = Torch.Functional.exp
          , log1p = Torch.Functional.log1p
          , log2 = Torch.Functional.log2
          , log = Torch.Functional.log
          , log10 = Torch.Functional.log10
          , pow = withScalar Torch.Functional.pow
          , powt = Torch.Functional.powt
          , relu = Torch.Functional.relu
          , elu = withScalar Torch.Functional.elu
          , selu = Torch.Functional.selu
          , celu = Torch.Functional.celu . realToFrac
          , sigmoid = Torch.Functional.sigmoid
          , softmax = Torch.Functional.softmax . Dim
          , logSoftmax = Torch.Functional.logSoftmax . Dim
          , threshold = \d v ->
              Torch.Functional.threshold (realToFrac d) $
                realToFrac v
          , sin = Torch.Functional.sin
          , cos = Torch.Functional.cos
          , tan = Torch.Functional.tan
          , sinh = Torch.Functional.sinh
          , cosh = Torch.Functional.cosh
          , tanh = Torch.Functional.tanh
          , sqrt = Torch.Functional.sqrt
          , gt = Torch.Functional.gt
          , lt = Torch.Functional.lt
          , ge = Torch.Functional.ge
          , le = Torch.Functional.le
          , eq = Torch.Functional.eq
          , take = Torch.Functional.take
          , maskedSelect = Torch.Functional.maskedSelect
          , nonzero = Torch.Functional.nonzero
          , isnan = Torch.Functional.isnan
          , isNonzero = Torch.Functional.isNonzero
          , isSameSize = Torch.Functional.isSameSize
          , isSigned = Torch.Functional.isSigned
          , squeezeAll = Torch.Functional.squeezeAll
          , squeezeDim = Torch.Functional.squeezeDim
          , ne = Torch.Functional.ne
          , mseLoss = Torch.Functional.mseLoss
          , dist = flip Torch.Functional.mseLoss . realToFrac
          , inverse = Torch.Functional.inverse
          , solve = Torch.Functional.solve
          , bitwiseNot = Torch.Functional.bitwiseNot
          , logicalNot = Torch.Functional.logicalNot
          , logicalXor = Torch.Functional.logicalXor
          , logicalAnd = Torch.Functional.logicalAnd
          , logicalOr = Torch.Functional.logicalOr
          , cat = Torch.Functional.cat . Dim
          , index = Torch.Functional.index
          , indexCopy = Torch.Functional.indexCopy
          , indexPut = Torch.Functional.indexPut
          , chunk = \i -> Torch.Functional.chunk i . Dim
          , clamp = \mn mx ->
              Torch.Functional.clamp (realToFrac mn) $
                realToFrac mx
          , clampMax = Torch.Functional.clampMax . realToFrac
          , clampMin = Torch.Functional.clampMin . realToFrac
          , sign = Torch.Functional.sign
          , transpose = \d1 -> Torch.Functional.transpose (Dim d1) . Dim
          , transpose2D = Torch.Functional.transpose2D
          , all = Torch.Functional.all
          , any = Torch.Functional.any
          , allDim = Torch.Functional.allDim . Dim
          , anyDim = Torch.Functional.anyDim . Dim
          , permute = Torch.Functional.permute
          , flatten = \d1 -> Torch.Functional.flatten (Dim d1) . Dim
          , flattenAll = Torch.Functional.flattenAll
          , softShrink = Torch.Functional.softShrink . realToFrac
          , stack = Torch.stack . Dim
          , logsumexp = Torch.logsumexp
          , trunc = Torch.trunc
          , unsqueeze = Torch.Functional.unsqueeze . Dim
          , split = \i -> Torch.Functional.split i . Dim
          , chainMatmul = Torch.chainMatmul
          , gelu = Torch.gelu
          , glu = Torch.glu . Dim
          , view = Torch.view
          , repeat = Torch.repeat
          }
    , properties =
        MkPropertyFuns
          { numel = Tensor.numel
          , size = Tensor.size
          , shape = Tensor.shape
          , dim = Tensor.dim
          , -- We could just try to implement `FromValue`/`ToValue` for `DType`,
            -- but we would need to account for invalid dtypes in the `ToValue`
            -- impl, e.g. by `error`ing. It's probably better to do the runtime
            -- error in that case.
            --
            -- The same considerations apply to `device` right below, since there
            -- may be invalid devices (for our purposes)
            dtype =
              VFun $ \case
                VCustom (VTensor t) -> case Torch.dtype t of
                  DType.Int64 -> pure $ VEnum enumDTypeHash "int"
                  DType.Float -> pure $ VEnum enumDTypeHash "float"
                  DType.Double -> pure $ VEnum enumDTypeHash "double"
                  DType.Bool -> pure $ VEnum enumDTypeHash "bool"
                  -- We don't support all possible tensor dtypes
                  d ->
                    throwM . RuntimeError $
                      "dtype: recevied unexpected dtype " <> show d
                _ -> throwM . RuntimeError $ "dtype: expected a tensor"
          , device =
              VFun $ \case
                VCustom (VTensor t) -> case Torch.device t of
                  Device CPU 0 -> pure $ VEnum enumDeviceHash "cpu"
                  Device CUDA 0 -> pure $ VEnum enumDeviceHash "cuda"
                  -- Again, there are several possibilities that we don't support
                  -- which need to be accounted for
                  d ->
                    throwM . RuntimeError $
                      "device: recevied unexpected device " <> show d
                _ -> throwM . RuntimeError $ "device: expected a tensor"
          }
    }

withDType ::
  (MonadThrow m) => String -> (DType -> Value (MlValue x) m) -> Value (MlValue x) m
withDType funName f =
  VFun $ \case
    VEnum _ e -> f <$> getDType funName e
    _ -> throwM . RuntimeError $ funName <> ": expecting a dtype enum"

getDType :: (MonadThrow m) => String -> Ident -> m DType
getDType funName = \case
  "int" -> pure DType.Int64
  "float" -> pure DType.Float
  "double" -> pure DType.Double
  "bool" -> pure DType.Bool
  s ->
    throwM . RuntimeError $
      unwords
        [ funName <> ":"
        , "unknown dtype"
        , show s
        ]

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

asTensorFun ::
  forall a x m.
  ( TensorLike a
  , MonadThrow m
  , FromValue (MlValue x) m a
  ) =>
  String ->
  Proxy a ->
  Value (MlValue x) m
asTensorFun funName _proxy =
  withDType funName $ \dtype ->
    VFun $
      fmap (VCustom . VTensor . Torch.toType dtype . Torch.asTensor @a)
        . fromValue

-- Lifts Hasktorch's `forward` to `IO` (via `evaluate`) so we can catch
-- any `CppStdException`s in case the Torchscript interpreter fails;
-- `forward` has a "pure" interface but internally uses `unsafePerformIO`,
-- so we need to `evaluate` it to be able to catch the exception in the Inferno
-- primitive
forwardIO :: ScriptModule -> [Tensor] -> IO [Tensor]
forwardIO m ts = unIV =<< evaluate (Torch.forward m (fmap IVTensor ts))
  where
    unIV :: IValue -> IO [Tensor]
    unIV = \case
      IVTensor t -> pure [t]
      IVTensorList tl -> pure tl
      IVTuple ivs -> concatMapM unIV ivs
      res -> throwM . RuntimeError $ "expected tensor result, got " <> show res

-- | Inferno prelude with a default @ML@ implementation (see 'defaultMlModule')
defaultMlPrelude ::
  forall m x.
  ( MonadIO m
  , MonadCatch m
  , Pretty x
  , Eq x
  ) =>
  Prelude.ModuleMap m (MlValue x)
defaultMlPrelude =
  Map.unionWith
    (error "Duplicate module name in builtinModules")
    (Prelude.builtinModules @m @(MlValue x))
    $ Compat.mkMlModule defaultMlModule

-- | Create an Inferno prelude with a custom @ML@ implementation. This can
-- be used to override certain primitives inside the @MlModule@
mkMlPrelude ::
  forall m x.
  ( MonadIO m
  , MonadCatch m
  , Pretty x
  , Eq x
  ) =>
  -- | Implementation of Inferno ML primitives
  MlModule (ImplEnvM m (MlValue x)) x ->
  Prelude.ModuleMap m (MlValue x)
mkMlPrelude =
  Map.unionWith
    (error "Duplicate module name in builtinModules")
    (Prelude.builtinModules @m @(MlValue x))
    . Compat.mkMlModule

data SomeScalar where
  SomeScalar :: forall a. (Torch.Scalar a) => a -> SomeScalar

-- Gets an existentially-wrapped `Torch.Scalar` from types that implement
-- `scalar` class in Inferno
getScalar :: Either3 Int Double Bool -> SomeScalar
getScalar = either SomeScalar $ either SomeScalar SomeScalar

withScalar ::
  (forall a. (Torch.Scalar a) => a -> Tensor -> Tensor) ->
  -- Types that implement `scalar` in Inferno
  Either3 Int Double Bool ->
  Tensor ->
  Tensor
withScalar f s t = case getScalar s of
  SomeScalar a -> f a t
