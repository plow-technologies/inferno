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
import Prelude hiding (tanh)

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
--
-- TODO: Add a lot more, re-rexport most of @Torch.Functional@. Will probably
-- need to split this into several sub-records though to avoid massive size
data MkFunctionalFuns tensor = MkFunctionalFuns
  { argmax :: Int -> Bool -> tensor -> tensor
  , softmax :: Int -> tensor -> tensor
  , stack :: Int -> [tensor] -> tensor
  , tanh :: tensor -> tensor
  , pow :: Int -> tensor -> tensor
  , unsqueeze :: Int -> tensor -> tensor
  , add :: tensor -> tensor -> tensor
  , sumAll :: tensor -> tensor
  , transpose2D :: tensor -> tensor
  , matmul :: tensor -> tensor -> tensor
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

  powT : int -> tensor -> tensor := ###pow###;

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
mkUnboundPrelude :: MkMlModule m tensor model mname x
mkUnboundPrelude =
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
          { argmax = unbound
          , softmax = unbound
          , stack = unbound
          , tanh = unbound
          , pow = unbound
          , unsqueeze = unbound
          , add = unbound
          , sumAll = unbound
          , transpose2D = unbound
          , matmul = unbound
          , mseLoss = unbound
          }
    }
  where
    unbound :: a
    unbound = error "Primitive is unbound"
