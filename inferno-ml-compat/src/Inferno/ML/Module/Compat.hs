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
    all,
    any,
    cos,
    cosh,
    div,
    exp,
    floor,
    log,
    max,
    min,
    repeat,
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
  , celu :: Double -> tensor -> tensor
  , sigmoid :: tensor -> tensor
  , softmax :: Int -> tensor -> tensor
  , logSoftmax :: Int -> tensor -> tensor
  , threshold :: Double -> Double -> tensor -> tensor
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
  , squeezeAll :: tensor -> tensor
  , squeezeDim :: Int -> tensor -> tensor
  , ne :: tensor -> tensor -> tensor
  , mseLoss :: tensor -> tensor -> tensor
  , dist :: Double -> tensor -> tensor
  , inverse :: tensor -> tensor
  , solve :: tensor -> tensor -> tensor
  , bitwiseNot :: tensor -> tensor
  , logicalNot :: tensor -> tensor
  , logicalXor :: tensor -> tensor -> tensor
  , logicalAnd :: tensor -> tensor -> tensor
  , logicalOr :: tensor -> tensor -> tensor
  , cat :: Int -> [tensor] -> tensor
  , index :: [tensor] -> tensor -> tensor
  , indexCopy :: Int -> tensor -> tensor -> tensor -> tensor
  , indexPut :: Bool -> [tensor] -> tensor -> tensor -> tensor
  , chunk :: Int -> Int -> tensor -> [tensor]
  , clamp :: Double -> Double -> tensor -> tensor
  , clampMax :: Double -> tensor -> tensor
  , clampMin :: Double -> tensor -> tensor
  , transpose :: Int -> Int -> tensor -> tensor
  , transpose2D :: tensor -> tensor
  , all :: tensor -> Bool
  , any :: tensor -> Bool
  , allDim :: Int -> Bool -> tensor -> tensor
  , anyDim :: Int -> Bool -> tensor -> tensor
  , permute :: [Int] -> tensor -> tensor
  , flatten :: Int -> Int -> tensor -> tensor
  , flattenAll :: tensor -> tensor
  , softShrink :: Double -> tensor -> tensor
  , stack :: Int -> [tensor] -> tensor
  , logsumexp :: Bool -> Int -> tensor -> tensor
  , trunc :: tensor -> tensor
  , unsqueeze :: Int -> tensor -> tensor
  , split :: Int -> Int -> tensor -> [tensor]
  , chainMatmul :: [tensor] -> tensor
  , gelu :: tensor -> tensor
  , view :: [Int] -> tensor -> tensor
  , repeat :: [Int] -> tensor -> tensor
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

  @doc Load a named, serialized model;
  loadModel : modelName -> model := ###!loadModel###;

  unsafeLoadScript : text -> model := ###unsafeLoadScript###;

  forward : model -> array of tensor -> array of tensor := ###!forward###;

  zeros : dtype{#int, #float, #double} -> array of int -> tensor := ###!zeros###;

  ones : dtype{#int, #float, #double} -> array of int -> tensor := ###!ones###;

  @doc An impure (pseudo)random tensor generator;
  randnIO : dtype{#int, #float, #double} -> array of int -> tensor := ###!randnIO###;

  @doc Move a tensor to a different device;
  toDevice : device{#cpu, #cuda} -> tensor -> tensor := ###!toDevice###;

  @doc Move a tensor to a different device, e.g. "cpu" or "cuda:0"
  (without checking validity of device name);
  toDeviceUnsafe : text -> tensor -> tensor := ###toDeviceUnsafe###;

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

module Tensor

  @doc Returns the mean value of all elements in the input tensor;
  mean : tensor -> tensor := ###mean###;

  @doc Returns the standard deviation of all elements in the input tensor;
  std : tensor -> tensor := ###std###;

  @doc Returns the variance of all elements in the input tensor;
  var : tensor -> tensor := ###var###;

  @doc Returns the sum of all elements in the input tensor;
  sumAll : tensor -> tensor := ###sumAll###;

  @doc Computes the element-wise absolute value of the given input tensor;
  abs : tensor -> tensor := ###abs###;

  @doc Computes the fractional portion of each element in input.
  `output = input - (floor . abs) input * (sign input)`;
  frac : tensor -> tensor := ###frac###;

  @doc `argmax i k t` is the argmax of tensor `t` along dimension `i`.
  `k` denotes whether the output tensor has dim retained or not.;
  argmax : int -> bool{#true, #false} -> tensor -> tensor := ###argmax###;

  @doc `add t1 t2` Each element of the tensor `t2` added to each element of the tensor `t1`.
  The resulting tensor is returned;
  add : tensor -> tensor -> tensor := ###add###;

  @doc `mul t1 t2` multiplies each element of the tensor `t2` to each element of the
  input tensor `t1` and returns a new resulting tensor;
  mul : tensor -> tensor -> tensor := ###mul###;

  @doc `sub t1 t2` performs element wise subtraction of tensor `t2` from input tensor
  `t1` and returns a new resulting tensor;
  sub : tensor -> tensor -> tensor := ###sub###;

  @doc `div t1 t2` performs element wise division of tensor `t2` from input tensor
  `t1` and returns a new resulting tensor;
  div : tensor -> tensor -> tensor := ###div###;

  @doc Returns a new tensor with the ceil of the elements of input, the smallest
  integer greater than or equal to each element;
  ceil : tensor -> tensor -> tensor := ###ceil###;

  @doc Returns a new tensor with the floor of the elements of input, the smallest
  integer less than or equal to each element;
  floor : tensor -> tensor -> tensor := ###floor###;

  @doc Returns the minimum value of all elements in the input tensor;
  min : tensor -> tensor -> tensor := ###min###;

  @doc Returns the maximum value of all elements in the input tensor;
  max : tensor -> tensor -> tensor := ###max###;

  @doc Returns the median value of all elements in the input tensor;
  median : tensor -> tensor -> tensor := ###median###;

  @doc `matmul t1 t2` is the matrix product of two tensors. The behavior depends
  on the dimensionality of the tensors as follows:
    - If both tensors are 1-dimensional, the dot product (scalar) is returned
    - If both arguments are 2-dimensional, the matrix-matrix product is returned
    - If `t1` is 1-dimensional and `t2` is 2-dimensional, a 1 is prepended to
      its dimension for the purpose of the matrix multiply. After the matrix
      multiply, the prepended dimension is removed
    - If `t1` is 2-dimensional and the `t2` is 1-dimensional, the matrix-vector product is returned
    - If both arguments are at least 1-dimensional and at least one argument is
      N-dimensional (where N > 2), then a batched matrix multiply is returned;
  matmul : tensor -> tensor -> tensor := ###matmul###;

  @doc `oneHot i t` is one hot encoding of the given input `t`. The encoding is
  based on the given number of classes `i`;
  oneHot : int -> tensor -> tensor := ###oneHot###;

  @doc Computes the error function of each element;
  erf : tensor -> tensor := ###erf###;

  @doc Computes the complementary error function of each element;
  erfc : tensor -> tensor := ###erfc###;

  @doc Computes the inverse error function of each element of input.
  The inverse error function is defined in the range `(-1, 1)(−1,1)` as:
  `erfinv(erf(x)) = x`;
  erfinv : tensor -> tensor := ###erfinv###;

  @doc Computes the logarithm of the gamma function on input;
  lgamma : tensor -> tensor := ###lgamma###;

  @doc Computes the logarithmic derivative of the gamma function on input;
  digamma : tensor -> tensor := ###digamma###;

  @doc `polygamma n t` computes the `n`th derivative of the digamma function on
  input. n≥0n≥0 is called the order of the polygamma function;
  polygamma : int -> tensor -> tensor := ###polygamma###;

  @doc `mvlgamma p t` computes the multivariate log-gamma function with dimension
  `p` element-wise. All elements must be greater than `(p-1)/2`, otherwise an
  error would be thrown;
  mvlgamma : int -> tensor -> tensor := ###mvlgamma###;

  @doc Returns a new tensor with the exponential of the elements of the input tensor;
  exp : tensor -> tensor := ###exp###;

  @doc `log1p t` returns a new tensor with the natural logarithm of `(1 + t)`;
  log1p : tensor -> tensor := ###log1p###;

  @doc Returns a new tensor with the logarithm to the base 2 of the elements of input;
  log2 : tensor -> tensor := ###log2###;

  @doc Returns a new tensor with the logarithm of the elements of input;
  log : tensor -> tensor := ###log###;

  @doc Returns a new tensor with the logarithm to the base 10 of the elements of input;
  log10 : tensor -> tensor := ###log10###;

  @doc `pow e t` takes the power of each element in `t` with exponent `e` and
  returns a tensor with the result;
  pow : int -> tensor -> tensor := ###pow###;

  @doc `powt t1 t2` takes the power of each element in input `t1` with exponent
  `t2` and returns a tensor with the result. Exponent `t2` is a tensor with the
  same number of elements as the input;
  powt : tensor -> tensor -> tensor := ###powt###;

  @doc Applies the rectified linear unit function element-wise;
  relu : tensor -> tensor := ###relu###;

  tanh : tensor -> tensor := ###tanh###;

  softmax : int -> tensor -> tensor := ###softmax###;

  @doc `stack i [t]` takes an array of tensors `t` and appends them along the
  dimension `i` in a new tensor;
  stack : int -> array of tensor -> tensor := ###stack###;

  transpose2D : tensor -> tensor := ###transpose2D###;

  mseLoss : tensor -> tensor -> tensor := ###mseLoss###;

  unsqueeze : int -> tensor -> tensor := ###unsqueeze###;

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
          , squeezeAll = unbound
          , squeezeDim = unbound
          , ne = unbound
          , mseLoss = unbound
          , dist = unbound
          , inverse = unbound
          , solve = unbound
          , bitwiseNot = unbound
          , logicalNot = unbound
          , logicalXor = unbound
          , logicalAnd = unbound
          , logicalOr = unbound
          , cat = unbound
          , index = unbound
          , indexCopy = unbound
          , indexPut = unbound
          , chunk = unbound
          , clamp = unbound
          , clampMax = unbound
          , clampMin = unbound
          , transpose = unbound
          , transpose2D = unbound
          , all = unbound
          , any = unbound
          , allDim = unbound
          , anyDim = unbound
          , permute = unbound
          , flatten = unbound
          , flattenAll = unbound
          , softShrink = unbound
          , stack = unbound
          , logsumexp = unbound
          , trunc = unbound
          , unsqueeze = unbound
          , split = unbound
          , chainMatmul = unbound
          , gelu = unbound
          , view = unbound
          , repeat = unbound
          }
    }
  where
    unbound :: a
    unbound = error "Primitive is unbound"
