{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Inferno.ML.Module.Compat where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Data (Typeable)
import Data.Text (Text)
import GHC.Generics
import Inferno.ML.Types.Value.Compat (MlValue, mlQuoter)
import Inferno.Module.Cast (FromValue (..), ToValue (..))
import Inferno.Module.Prelude (ModuleMap)
import Inferno.Types.Value (ImplEnvM, Value)
import Prettyprinter (Pretty)
import Prelude hiding
  ( abs,
    cos,
    cosh,
    div,
    exp,
    floor,
    log,
    max,
    min,
    sin,
    sinh,
    sqrt,
    take,
    tan,
    tanh,
  )

-- | Record-of-functions to generate the ML prelude, given implementations of
-- each primitive within the sub-records. This is broken up into smaller internal
-- records to avoid a huge single one
data MkMlModule m tensor model mname x = MkMlModule
  { models :: MkModelFuns m tensor model mname x
  , devices :: MkDeviceFuns m tensor model mname x
  , factories :: MkFactoryFuns m tensor model mname x
  , conversions :: MkConversionFuns m tensor model mname x
  , functional :: MkFunctionalFuns tensor
  }
  deriving (Generic)

-- | Primitives related to models (Torchscript modules)
--
-- NOTE: For Inferno primitive documentation, see the Inferno module
data MkModelFuns m tensor model mname x = MkModelFuns
  { loadModel :: Value (MlValue tensor model mname x) m
  , forward :: Value (MlValue tensor model mname x) m
  , unsafeLoadScript :: Text -> model
  }
  deriving (Generic)

-- | Device-related primitives, e.g. moving tensors
--
-- NOTE: For Inferno primitive documentation, see the Inferno module
data MkDeviceFuns m tensor model mname x = MkDeviceFuns
  { toDevice :: Value (MlValue tensor model mname x) m
  , toDeviceUnsafe :: Text -> tensor -> tensor
  }
  deriving (Generic)

-- | Tensor factory primitives
--
-- NOTE: For Inferno primitive documentation, see the Inferno module
data MkFactoryFuns m tensor model mname x = MkFactoryFuns
  { zeros :: Value (MlValue tensor model mname x) m
  , ones :: Value (MlValue tensor model mname x) m
  , randnIO :: Value (MlValue tensor model mname x) m
  }
  deriving (Generic)

-- | Primitives to convert to and from tensors
--
-- NOTE: For Inferno primitive documentation, see the Inferno module
data MkConversionFuns m tensor model mname x = MkConversionFuns
  { toType :: Value (MlValue tensor model mname x) m
  , asTensor0 :: Value (MlValue tensor model mname x) m
  , asTensor1 :: Value (MlValue tensor model mname x) m
  , asTensor2 :: Value (MlValue tensor model mname x) m
  , asTensor3 :: Value (MlValue tensor model mname x) m
  , asTensor4 :: Value (MlValue tensor model mname x) m
  , asDouble :: tensor -> Double
  , asArray1 :: tensor -> [Double]
  , asArray2 :: tensor -> [[Double]]
  , asArray3 :: tensor -> [[[Double]]]
  , asArray4 :: tensor -> [[[[Double]]]]
  }
  deriving (Generic)

-- | Tensor operation primitives, corresponding to those in @Torch.Functional@
--
-- NOTE: For Inferno primitive documentation, see the Inferno module
data MkFunctionalFuns tensor = MkFunctionalFuns
  { mean :: tensor -> tensor
  , std :: tensor -> tensor
  , var :: tensor -> tensor
  , sumAll :: tensor -> tensor
  , abs :: tensor -> tensor
  , frac :: tensor -> tensor
  , argmax :: Int -> Bool -> tensor -> tensor
  , add :: tensor -> tensor -> tensor
  , mul :: tensor -> tensor -> tensor
  , sub :: tensor -> tensor -> tensor
  , div :: tensor -> tensor -> tensor
  , ceil :: tensor -> tensor
  , floor :: tensor -> tensor
  , min :: tensor -> tensor
  , max :: tensor -> tensor
  , median :: tensor -> tensor
  , matmul :: tensor -> tensor -> tensor
  , oneHot :: Int -> tensor -> tensor
  , erf :: tensor -> tensor
  , erfc :: tensor -> tensor
  , erfinv :: tensor -> tensor
  , lgamma :: tensor -> tensor
  , digamma :: tensor -> tensor
  , polygamma :: Int -> tensor -> tensor
  , mvlgamma :: Int -> tensor -> tensor
  , exp :: tensor -> tensor
  , log1p :: tensor -> tensor
  , log2 :: tensor -> tensor
  , log :: tensor -> tensor
  , log10 :: tensor -> tensor
  , pow :: Int -> tensor -> tensor
  , powt :: tensor -> tensor -> tensor
  , relu :: tensor -> tensor
  , elu :: Int -> tensor -> tensor
  , selu :: tensor -> tensor
  , celu :: Float -> tensor -> tensor
  , sigmoid :: tensor -> tensor
  , softmax :: Int -> tensor -> tensor
  , logSoftmax :: Int -> tensor -> tensor
  , threshold :: Float -> Float -> tensor -> tensor
  , sin :: tensor -> tensor
  , cos :: tensor -> tensor
  , tan :: tensor -> tensor
  , sinh :: tensor -> tensor
  , cosh :: tensor -> tensor
  , tanh :: tensor -> tensor
  , sqrt :: tensor -> tensor
  , gt :: tensor -> tensor -> tensor
  , lt :: tensor -> tensor -> tensor
  , ge :: tensor -> tensor -> tensor
  , le :: tensor -> tensor -> tensor
  , eq :: tensor -> tensor -> tensor
  , take :: tensor -> tensor -> tensor
  , maskedSelect :: tensor -> tensor -> tensor
  , nonzero :: tensor -> tensor
  , isnan :: tensor -> tensor
  , isNonzero :: tensor -> Bool
  , isSameSize :: tensor -> tensor -> Bool
  , isSigned :: tensor -> Bool
  , stack :: Int -> [tensor] -> tensor
  , unsqueeze :: Int -> tensor -> tensor
  , transpose2D :: tensor -> tensor
  , mseLoss :: tensor -> tensor -> tensor
  }
  deriving (Generic)

mkMlModule ::
  forall m tensor model mname x.
  ( MonadThrow m
  , MonadIO m
  , Typeable tensor
  , Show tensor
  , Show model
  , Show mname
  , Pretty x
  , Pretty (MlValue tensor model mname x)
  , FromValue
      (MlValue tensor model mname x)
      (ImplEnvM m (MlValue tensor model mname x))
      tensor
  , FromValue
      (MlValue tensor model mname x)
      (ImplEnvM m (MlValue tensor model mname x))
      model
  , FromValue
      (MlValue tensor model mname x)
      (ImplEnvM m (MlValue tensor model mname x))
      mname
  , ToValue
      (MlValue tensor model mname x)
      (ImplEnvM m (MlValue tensor model mname x))
      tensor
  , ToValue
      (MlValue tensor model mname x)
      (ImplEnvM m (MlValue tensor model mname x))
      model
  , ToValue
      (MlValue tensor model mname x)
      (ImplEnvM m (MlValue tensor model mname x))
      mname
  ) =>
  MkMlModule (ImplEnvM m (MlValue tensor model mname x)) tensor model mname x ->
  ModuleMap m (MlValue tensor model mname x)
mkMlModule mk =
  [mlQuoter|

module ML

  enum dtype := #int | #float | #double;

  enum device := #cpu | #cuda;

  zeros : dtype{#int, #float, #double} -> array of int -> tensor := ###!zeros###;

  ones : dtype{#int, #float, #double} -> array of int -> tensor := ###!ones###;

  add : tensor -> tensor -> tensor := ###add###;

  toType : dtype{#int, #float, #double} -> tensor -> tensor := ###!toType###;

  asTensor0 : dtype{#int, #float, #double} -> double -> tensor := ###!asTensor0###;

  asTensor1 : dtype{#int, #float, #double} -> array of double -> tensor := ###!asTensor1###;

  asTensor2 : dtype{#int, #float, #double} -> array of (array of double) -> tensor := ###!asTensor2###;

  asTensor3 : dtype{#int, #float, #double} -> array of (array of (array of double)) -> tensor := ###!asTensor3###;

  asTensor4 : dtype{#int, #float, #double} -> array of (array of (array of (array of double))) -> tensor := ###!asTensor4###;

  asDouble : tensor -> double := ###asDouble###;

  asArray1 : tensor -> array of double := ###asArray1###;

  asArray2 : tensor -> array of (array of double) := ###asArray2###;

  asArray3 : tensor -> array of (array of (array of double)) := ###asArray3###;

  asArray4 : tensor -> array of (array of (array of (array of double))) := ###asArray4###;

  sumAll : tensor -> tensor := ###sumAll###;

  powt : int -> tensor -> tensor := ###powt###;

  tanH : tensor -> tensor := ###tanh###;

  @doc `argmax i k t` is the argmax of tensor `t` along dimension `i`. `k` denotes whether the output tensor has dim retained or not.;
  argmax : int -> bool{#true, #false} -> tensor -> tensor := ###argmax###;

  softmax : int -> tensor -> tensor := ###softmax###;

  @doc `stack i [t]` takes an array of tensors `t` and appends them along the dimension i in a new tensor;
  stack : int -> array of tensor -> tensor := ###stack###;

  transpose2D : tensor -> tensor := ###transpose2D###;

  matmul : tensor -> tensor -> tensor := ###matmul###;

  mseLoss : tensor -> tensor -> tensor := ###mseLoss###;

  unsqueeze : int -> tensor -> tensor := ###unsqueeze###;

  @doc An impure (pseudo)random tensor generator;
  randnIO : dtype{#int, #float, #double} -> array of int -> tensor := ###!randnIO###;

  @doc Move a tensor to a different device;
  toDevice : device{#cpu, #cuda} -> tensor -> tensor := ###!toDevice###;

  @doc Move a tensor to a different device, e.g. "cpu" or "cuda:0" (without checking validity of device name);
  toDeviceUnsafe : text -> tensor -> tensor := ###toDeviceUnsafe###;

  @doc Load a named, serialized model;
  loadModel : modelName -> model := ###!loadModel###;

  unsafeLoadScript : text -> model := ###unsafeLoadScript###;

  forward : model -> array of tensor -> array of tensor := ###!forward###;

|]
  where
    -- Unfortunately the Inferno QQ parser can't handle overloaded record dots,
    -- so the next best thing is to just use wildcards scoped to this function
    MkModelFuns{..} = mk.models
    MkDeviceFuns{..} = mk.devices
    MkFactoryFuns{..} = mk.factories
    MkConversionFuns{..} = mk.conversions
    MkFunctionalFuns{..} = mk.functional

-- | Given concrete types, this will create a 'MkPrelude' that is suitable for
-- type-checking purposes, but which will throw an error if evaluated. This is
-- useful when creating a prelude that does not depend directly on Hasktorch
--
-- See the @inferno-ml@ package\'s prelude for a true implementation
mkUnboundModule :: MkMlModule m tensor model mname x
mkUnboundModule =
  MkMlModule
    { models =
        MkModelFuns
          { loadModel = unbound
          , forward = unbound
          , unsafeLoadScript = unbound
          }
    , devices =
        MkDeviceFuns
          { toDevice = unbound
          , toDeviceUnsafe = unbound
          }
    , factories =
        MkFactoryFuns
          { zeros = unbound
          , ones = unbound
          , randnIO = unbound
          }
    , conversions =
        MkConversionFuns
          { toType = unbound
          , asTensor0 = unbound
          , asTensor1 = unbound
          , asTensor2 = unbound
          , asTensor3 = unbound
          , asTensor4 = unbound
          , asDouble = unbound
          , asArray1 = unbound
          , asArray2 = unbound
          , asArray3 = unbound
          , asArray4 = unbound
          }
    , functional =
        MkFunctionalFuns
          { mean = unbound
          , std = unbound
          , var = unbound
          , sumAll = unbound
          , abs = unbound
          , frac = unbound
          , argmax = unbound
          , add = unbound
          , sub = unbound
          , div = unbound
          , ceil = unbound
          , mul = unbound
          , floor = unbound
          , min = unbound
          , max = unbound
          , median = unbound
          , matmul = unbound
          , oneHot = unbound
          , erf = unbound
          , erfc = unbound
          , erfinv = unbound
          , lgamma = unbound
          , digamma = unbound
          , polygamma = unbound
          , mvlgamma = unbound
          , exp = unbound
          , log1p = unbound
          , log2 = unbound
          , log = unbound
          , log10 = unbound
          , pow = unbound
          , powt = unbound
          , relu = unbound
          , selu = unbound
          , elu = unbound
          , celu = unbound
          , sigmoid = unbound
          , softmax = unbound
          , logSoftmax = unbound
          , threshold = unbound
          , sin = unbound
          , cos = unbound
          , tan = unbound
          , sinh = unbound
          , cosh = unbound
          , tanh = unbound
          , sqrt = unbound
          , gt = unbound
          , lt = unbound
          , ge = unbound
          , le = unbound
          , eq = unbound
          , take = unbound
          , maskedSelect = unbound
          , nonzero = unbound
          , isnan = unbound
          , isNonzero = unbound
          , isSameSize = unbound
          , isSigned = unbound
          , stack = unbound
          , unsqueeze = unbound
          , transpose2D = unbound
          , mseLoss = unbound
          }
    }
  where
    unbound :: a
    unbound = error "Primitive is unbound"
