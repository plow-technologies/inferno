{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wwarn #-}

module Inferno.ML.Module.Prelude
  ( defaultMlPrelude,
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
import qualified Inferno.ML.Module.Compat as Compat
import Inferno.ML.Types.Value
import Inferno.Module.Cast (FromValue (fromValue), ToValue (toValue))
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

type MkModule m x = Compat.MkMlModule m Tensor ScriptModule ModelName x

defaultMlModule ::
  forall m x.
  ( MonadIO m
  , MonadCatch m
  , Pretty x
  , Eq x
  ) =>
  MkModule m x
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
                    VFun $
                      fmap (toValue @_ @_ @Tensor . Torch.toDevice dev)
                        . fromValue @_ @_ @Tensor
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
          { argmax =
              \i -> Torch.Functional.argmax (Dim i) . bool RemoveDim KeepDim
          , softmax = Torch.Functional.softmax . Dim
          , stack = Torch.stack . Dim
          , tanh = Torch.Functional.tanh
          , pow = Torch.Functional.pow
          , unsqueeze = Torch.Functional.unsqueeze . Dim
          , add = Torch.Functional.add
          , sumAll = Torch.Functional.sumAll
          , transpose2D = Torch.Functional.transpose2D
          , matmul = Torch.Functional.matmul
          , mseLoss = Torch.Functional.mseLoss
          }
    }

getDType :: (MonadThrow m) => String -> Ident -> m DType
getDType funName = \case
  "int" -> pure DType.Int64
  "float" -> pure DType.Float
  "double" -> pure DType.Double
  s ->
    throwM . RuntimeError $
      unwords
        [ funName <> ":"
        , "unknown dtype "
        , show s
        ]

withDType ::
  (MonadThrow m) => String -> (DType -> Value (MlValue x) m) -> Value (MlValue x) m
withDType funName f =
  VFun $ \case
    VEnum _ e -> f <$> getDType funName e
    _ -> throwM . RuntimeError $ funName <> ": expecting a dtype enum"

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

mkMlPrelude ::
  forall m x.
  ( MonadIO m
  , MonadCatch m
  , Pretty x
  , Eq x
  ) =>
  -- | Implementation of Inferno ML primitives
  MkModule (ImplEnvM m (MlValue x)) x ->
  Prelude.ModuleMap m (MlValue x)
mkMlPrelude =
  Map.unionWith
    (error "Duplicate module name in builtinModules")
    (Prelude.builtinModules @m @(MlValue x))
    . Compat.mkMlModule
