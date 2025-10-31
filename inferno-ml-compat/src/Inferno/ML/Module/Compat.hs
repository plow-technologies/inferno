{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Inferno.ML.Module.Compat where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Data (Typeable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Inferno.ML.Types.Value.Compat (MlValue, mlQuoter)
import Inferno.Module.Cast (Either3, FromValue (..), ToValue (..))
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
  , properties :: MkPropertyFuns m tensor model mname x
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
  , addScalar :: ScalarValue -> tensor -> tensor
  , subScalar :: ScalarValue -> tensor -> tensor
  , mulScalar :: ScalarValue -> tensor -> tensor
  , divScalar :: ScalarValue -> tensor -> tensor
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
  , pow :: ScalarValue -> tensor -> tensor
  , powt :: tensor -> tensor -> tensor
  , relu :: tensor -> tensor
  , elu :: ScalarValue -> tensor -> tensor
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
  , sign :: tensor -> tensor
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
  , glu :: Int -> tensor -> tensor
  , view :: [Int] -> tensor -> tensor
  , repeat :: [Int] -> tensor -> tensor
  , roll :: tensor -> Int -> Int -> tensor
  }
  deriving (Generic)

-- | Tensor properties, corresponding to those in @Torch.Tensor@
--
-- NOTE: For Inferno primitive documentation, see the Inferno module
data MkPropertyFuns m tensor model mname x = MkPropertyFuns
  { numel :: tensor -> Int
  , size :: Int -> tensor -> Int
  , shape :: tensor -> [Int]
  , dim :: tensor -> Int
  , dtype :: Value (MlValue tensor model mname x) m
  , device :: Value (MlValue tensor model mname x) m
  , -- NOTE: quantile is in `MkPropertyFuns` (not `MkFunctionalFuns`, where it belongs)
    -- because it needs to be effectful in order to convert the interpolation enum to
    -- a `String`. It's easier to put it here rather than change `MkFunctionalFuns`
    quantile :: Value (MlValue tensor model mname x) m
  , -- NOTE: `dquantile` is in `MkPropertyFuns` for the same reason as `quantile`
    dquantile :: Value (MlValue tensor model mname x) m
  }
  deriving (Generic)

mkMlModule ::
  forall m tensor model mname x.
  ( MonadThrow m
  , MonadCatch m
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

  enum dtype := #int | #float | #double | #bool;

  enum device := #cpu | #cuda;

  enum qinterp := #linear | #lower | #higher | #nearest | #midpoint;

  @doc Load a named, serialized model;
  loadModel : modelName -> model := ###!loadModel###;

  unsafeLoadScript : text -> model := ###unsafeLoadScript###;

  forward : model -> array of tensor -> array of tensor := ###!forward###;

  zeros : dtype{#int, #float, #double, #bool} -> array of int -> tensor := ###!zeros###;

  ones : dtype{#int, #float, #double, #bool} -> array of int -> tensor := ###!ones###;

  @doc An impure (pseudo)random tensor generator;
  randnIO : dtype{#int, #float, #double} -> array of int -> tensor := ###!randnIO###;

  @doc Move a tensor to a different device;
  toDevice : device{#cpu, #cuda} -> tensor -> tensor := ###!toDevice###;

  @doc Move a tensor to a different device, e.g. "cpu" or "cuda:0"
  (without checking validity of device name);
  toDeviceUnsafe : text -> tensor -> tensor := ###toDeviceUnsafe###;

  toType : dtype{#int, #float, #double, #bool} -> tensor -> tensor := ###!toType###;

  asTensor0 : dtype{#int, #float, #double, #bool} -> double -> tensor := ###!asTensor0###;

  asTensor1 : dtype{#int, #float, #double, #bool} -> array of double -> tensor := ###!asTensor1###;

  asTensor2 : dtype{#int, #float, #double, #bool} -> array of (array of double) -> tensor := ###!asTensor2###;

  asTensor3 : dtype{#int, #float, #double, #bool} -> array of (array of (array of double)) -> tensor := ###!asTensor3###;

  asTensor4 : dtype{#int, #float, #double, #bool} -> array of (array of (array of (array of double))) -> tensor := ###!asTensor4###;

  asDouble : tensor -> double := ###asDouble###;

  asArray1 : tensor -> array of double := ###asArray1###;

  asArray2 : tensor -> array of (array of double) := ###asArray2###;

  asArray3 : tensor -> array of (array of (array of double)) := ###asArray3###;

  asArray4 : tensor -> array of (array of (array of (array of double))) := ###asArray4###;

module Tensor
  @doc Returns the total number of elements in the input tensor;
  numel : tensor -> int := ###numel###;

  @doc `size dim t` returns the size of the given `dim` of the input `t`;
  size : int -> tensor -> int := ###size###;

  @doc Returns the shape of the tensor;
  shape : tensor -> array of int := ###shape###;

  @doc Returns the dimensions of the input tensor;
  dim : tensor -> int := ###dim###;

  @doc Returns the device on which the tensor is currently allocated;
  device : tensor -> device{#cpu, #cuda} := ###!device###;

  @doc Returns the data type of the input tensor;
  dtype : tensor -> dtype{#int, #float, #double, #bool} := ###!dtype###;

  @doc `quantile t q dim keepdim interp` computes the `q`-th quantile of the tensor `t`
  along dimension `dim`. If `keepdim` is true, the output tensor has the same number of
  dimensions as `t`, with the reduced dimension of size 1. The interpolation method is
  specified by `interp`;
  quantile :
    tensor
    -> tensor
    -> int
    -> bool{#true, #false}
    -> qinterp{#linear, #lower, #higher, #nearest, #midpoint}
    -> tensor
    := ###!quantile###;

  @doc `dquantile t q dim keepdim interp` computes the `q`-th quantile of the tensor `t`
  along dimension `dim`. If `keepdim` is true, the output tensor has the same number of
  dimensions as `t`, with the reduced dimension of size 1. The interpolation method is
  specified by `interp`. This variant takes a `double` for the quantile value instead of a `tensor`;
  dquantile :
    tensor
    -> double
    -> int
    -> bool{#true, #false}
    -> qinterp{#linear, #lower, #higher, #nearest, #midpoint}
    -> tensor
    := ###!dquantile###;

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

  @doc `add t1 t2` adds each element of the tensor `t2` to each element of the
  tensor `t1` and returns a new resulting tensor;
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
  ceil : tensor -> tensor := ###ceil###;

  @doc Returns a new tensor with the floor of the elements of input, the smallest
  integer less than or equal to each element;
  floor : tensor -> tensor := ###floor###;

  @doc Returns the minimum value of all elements in the input tensor;
  min : tensor -> tensor := ###min###;

  @doc Returns the maximum value of all elements in the input tensor;
  max : tensor -> tensor := ###max###;

  @doc Returns the median value of all elements in the input tensor;
  median : tensor -> tensor := ###median###;

  @doc `addScalar summand t` adds each element of `t` with the scalar `summand`
  and returns a new resulting tensor;
  addScalar : forall 'a. {requires scalar on 'a} => 'a -> tensor -> tensor := ###addScalar###;

  @doc `subScalar subtrahend t` subtracts each element of `t` with the scalar
  `subtrahend` and returns a new resulting tensor;
  subScalar : forall 'a. {requires scalar on 'a} => 'a -> tensor -> tensor := ###subScalar###;

  @doc `mulScalar multiplier t` multiplies each element of `t` with the scalar
  `multiplier` and returns a new resulting tensor;
  mulScalar : forall 'a. {requires scalar on 'a} => 'a -> tensor -> tensor := ###mulScalar###;

  @doc `divScalar divisor t` divides each element of `t` with the scalar `divisor`
  and returns a new resulting tensor;
  divScalar : forall 'a. {requires scalar on 'a} => 'a -> tensor -> tensor := ###divScalar###;

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
  pow : forall 'a. {requires scalar on 'a} => 'a -> tensor -> tensor := ###pow###;

  @doc `powt t1 t2` takes the power of each element in input `t1` with exponent
  `t2` and returns a tensor with the result. Exponent `t2` is a tensor with the
  same number of elements as the input;
  powt : tensor -> tensor -> tensor := ###powt###;

  @doc Applies the rectified linear unit function element-wise;
  relu : tensor -> tensor := ###relu###;

  @doc `elu α t` applies exponential linear unit function element-wise,
  with alpha input `α`, `ELU(x) = max(0, x) + min(0, α * (x^2 - 1))`;
  elu : forall 'a. {requires scalar on 'a} => 'a -> tensor -> tensor := ###elu###;

  @doc Applies element-wise, `SELU(x) = scale * (max(0, x) + min(0, α * (exp(x) - 1)))`,
  with `α`=1.6732632423543772848170429916717 and `scale`=1.0507009873554804934193349852946;
  selu : tensor -> tensor := ###selu###;

  @doc `celu α t` Applies element-wise `CELU(x) = max(0, x) + min(0, α * (exp(x/α) - 1))`;
  celu : double -> tensor -> tensor := ###celu###;

  @doc Applies element-wise sigmoid function;
  sigmoid : tensor -> tensor := ###sigmoid###;

  @doc `softmax dim t` applies a softmax function. It is applied to all slices along `dim`,
  and will re-scale them so that the elements lie in the range `[0, 1]` and sum to 1;
  softmax : int -> tensor -> tensor := ###softmax###;

  @doc `logSoftmax dim t` applies a softmax followed by a logarithm. While mathematically
  equivalent to `log(softmax(t))`, doing these two operations separately is slower, and
  numerically unstable. This function uses an alternative formulation to compute the output
  and gradient correctly;
  logSoftmax : int -> tensor -> tensor := ###logSoftmax###;

  @doc `threshold d value t` thresholds each element of the input according to `d`;
  threshold : double -> double -> tensor -> tensor := ###threshold###;

  @doc Returns a new tensor with the sine of the elements of input;
  sin : tensor -> tensor := ###sin###;

  @doc Returns a new tensor with the cos of the elements of input;
  cos : tensor -> tensor := ###cos###;

  @doc Returns a new tensor with the tangent of the elements of input;
  tan : tensor -> tensor := ###tan###;

  @doc Returns a new tensor with the hyperbolic sine of the elements of input;
  sinh : tensor -> tensor := ###sinh###;

  @doc Returns a new tensor with the hyperbolic cos of the elements of input;
  cosh : tensor -> tensor := ###cosh###;

  @doc Returns a new tensor with the hyperbolic tangent of the elements of input;
  tanh : tensor -> tensor := ###tanh###;

  @doc Returns a new tensor with the square root of the elements of input;
  sqrt : tensor -> tensor := ###sqrt###;

  @doc `gt t1 t2` computes `t1 > t2` element-wise;
  gt : tensor -> tensor -> tensor := ###gt###;

  @doc `lt t1 t2` computes `t1 < t2` element-wise;
  lt : tensor -> tensor -> tensor := ###lt###;

  @doc `ge t1 t2` computes `t1 ≥ t2` element-wise;
  ge : tensor -> tensor -> tensor := ###ge###;

  @doc `le t1 t2` computes `t1 ≤ t2` element-wise;
  le : tensor -> tensor -> tensor := ###le###;

  @doc `eq t1 t2` computes `t1 == t2` element-wise;
  eq : tensor -> tensor -> tensor := ###eq###;

  @doc `ne t1 t2` computes `t1 ≠ t2` element-wise;
  ne : tensor -> tensor -> tensor := ###ne###;

  @doc `take indices t` returns a new tensor with the elements of input at the
  given `indices`. The input tensor is treated as if it were viewed as a 1-D tensor.
  The result takes the same shape as the indices;
  take : tensor -> tensor -> tensor := ###take###;

  @doc `maskedSelect mask t` returns a new 1-D tensor which indexes the input
  tensor according to the boolean mask `mask` which is a tensor with dtype `#bool`.
  The shapes of the mask tensor and the input tensor don’t need to match, but they
  must be broadcastable;
  maskedSelect : tensor -> tensor -> tensor := ###maskedSelect###;

  @doc Returns a tensor containing the indices of all non-zero elements of input.
  Each row in the result contains the indices of a non-zero element in input.
  The result is sorted lexicographically, with the last index changing the fastest;
  nonzero : tensor -> tensor := ###nonzero###;

  @doc Returns a new tensor with dtype `#bool` whose elements represent if each
  element of the input is NaN or not. Complex values are considered NaN when either
  their real and/or imaginary part is NaN;
  isnan : tensor -> tensor := ###isnan###;

  @doc Returns true if the input is a single element tensor which is not equal
  to zero after type conversions;
  isNonzero : tensor -> bool{#true, #false} := ###isNonzero###;

  isSameSize : tensor -> tensor -> bool{#true, #false} := ###isSameSize###;

  @doc Returns true if the data type of the input is a signed type;
  isSigned : tensor -> bool{#true, #false} := ###isSigned###;

  @doc Returns a tensor with all specified dimensions of the input of size 1
  removed. For example, if the input `t` is of shape: `(A×1×B×C×1×D)` then
  `squeezeAll t` will be of shape: `(A×B×C×D)`;
  squeezeAll : tensor -> tensor := ###squeezeAll###;

  @doc Similar to `Tensor.squeezeAll`, but only squeezes along the provided dim.
  For example, if input `t` is of shape: `(A×1×B)`, `squeeze t 0` leaves the tensor
  unchanged, but `squeeze t 1` will squeeze the tensor to the shape `(A×B)`;
  squeezeDim : int -> tensor -> tensor := ###squeezeDim###;

  @doc `mseLoss target t` creates a criterion that measures the mean squared error
  (squared L2 norm) between each element in the input `t` and `target`;
  mseLoss : tensor -> tensor -> tensor := ###mseLoss###;

  @doc `dist p t1 t2` returns the `p`-norm of `(t1 - t2)`. The shapes of `t1`
  and `t2` must be broadcastable;
  dist : double -> tensor -> tensor := ###dist###;

  @doc Takes the inverse of the square matrix input. The input can be batches of
  2D square tensors, in which case this function would return a tensor composed
  of individual inverses;
  inverse : tensor -> tensor := ###inverse###;

  @doc `solve t square` returns the solution to the system of linear equations
  represented by `AX = BAX=B` and the LU factorization of A;
  solve : tensor -> tensor -> tensor := ###solve###;

  @doc Computes the bitwise NOT of the given input tensor. The input tensor must
  be of integral or Boolean types. For bool tensors, it computes the logical NOT;
  bitwiseNot : tensor -> tensor := ###bitwiseNot###;

  @doc Computes the element-wise logical NOT of the given input tensor. The output
  tensor will have the `#bool` dtype. If the input tensor is not a bool tensor,
  zeros are treated as false and non-zeros are treated as true;
  logicalNot : tensor -> tensor := ###logicalNot###;

  @doc Computes the element-wise logical XOR of the given input tensors.
  Zeros are treated as false and nonzeros are treated as true;
  logicalXor : tensor -> tensor -> tensor := ###logicalXor###;

  @doc Computes the element-wise logical AND of the given input tensors.
  Zeros are treated as false and nonzeros are treated as true;
  logicalAnd : tensor -> tensor -> tensor := ###logicalAnd###;

  @doc Computes the element-wise logical OR of the given input tensors.
  Zeros are treated as false and nonzeros are treated as true;
  logicalOr : tensor -> tensor -> tensor := ###logicalOr###;

  @doc `cat dim ts` concatenates `ts` in the given `dim`. All tensors must either
  have the same shape (except in the concatenating dimension) or be empty;
  cat : int -> array of tensor -> tensor := ###cat###;

  index : array of tensor -> tensor -> tensor := ###index###;

  @doc `indexCopy dim index source t` copies the elements of `source` into `t`
  (out-of-place) by selecting the indices in the order given in the `index`
  tensor. For example, if `dim == 0` and `index[i] == j`, then the `i`th row of
  `source` is copied to the `j`th row of `t`. The `dim`th dimension of `source`
  must have the same size as the length of `index` (which must be a vector),
  and all other dimensions must match `t`, or an error will be raised;
  indexCopy : int -> tensor -> tensor -> tensor -> tensor := ###indexCopy###;

  @doc `indexPut accumulate indices source t` Puts values from the tensor `source`
  into `t` (out-of-place) using the indices specified in `indices` (which is a
  tuple of tensors). The expression `Tensor.indexPut #true indices source t` is
  equivalent to `t[indices] = source`. If `accumulate` is true, the elements in
  `source` are added to `t`. If accumulate is false, the behavior is undefined if
  indices contain duplicate elements;
  indexPut : bool{#true, #false} -> array of tensor -> tensor -> tensor -> tensor := ###indexPut###;

  @doc `chunk chunks dim t` splits a tensor into a specific number of chunks.
  The last chunk will be smaller if the size of `t` along the given dimension
  `dim` is not divisible by `chunks`;
  chunk : int -> int -> tensor -> array of tensor := ###chunk###;

  @doc `clamp min max t` clamps all elements in input into the range `[ min, max ]`
  and returns the resulting tensor;
  clamp : double -> double -> tensor -> tensor := ###clamp###;

  @doc `clampMax max t` clamps all elements in `t` to be smaller or equal to `max`;
  clampMax : double -> tensor -> tensor := ###clampMax###;

  @doc `clampMin min t` clamps all elements in `t` to be larger or equal to `min`;
  clampMin : double -> tensor -> tensor := ###clampMin###;

  @doc Returns a new tensor with the signs of the elements of the input;
  sign : tensor -> tensor := ###sign###;

  @doc `transpose dim1 dim2 t` returns a tensor that is a transposed version of `t`.
  The given dimensions `dim1` and `dim2` are swapped;
  transpose : int -> int -> tensor -> tensor := ###transpose###;

  @doc Special case of `Tensor.transpose` for a 2D tensor (where `dim1 = 0` and
  `dim2 = 1`);
  transpose2D : tensor -> tensor := ###transpose2D###;

  @doc Returns true if all elements in the tensor are true, false otherwise;
  all : tensor -> bool{#true, #false} := ###all###;

  @doc Returns true if any element in the tensor is true, false otherwise;
  any : tensor -> bool{#true, #false} := ###any###;

  @doc `allDim dim keepdim t` returns true if all elements in each row of `t`
  in the given dimension `dim` are true, false otherwise. If `keepdim` is `#true`,
  the output tensor is of the same size as `t` except in the dimension `dim` where
  it is of size 1. Otherwise, `dim` is squeezed, resulting in the output tensor
  having 1 fewer dimension than `t`;
  allDim : int -> bool{#true, #false} -> tensor -> tensor := ###allDim###;

  @doc `anyDim dim keepdim t` returns true if any elements in each row of `t`
  in the given dimension `dim` are true, false otherwise. If `keepdim` is `#true`,
  the output tensor is of the same size as `t` except in the dimension `dim` where
  it is of size 1. Otherwise, `dim` is squeezed, resulting in the output tensor
  having 1 fewer dimension than `t`;
  anyDim : int -> bool{#true, #false} -> tensor -> tensor := ###anyDim###;

  @doc `permute dims t` permutes the dimensions of this tensor, where `dims`
  corresponds to the ordering of dimensions to permute with;
  permute : array of int -> tensor -> tensor := ###permute###;

  @doc `flatten startdim enddim t` flattens `t` by reshaping it into a
  one-dimensional tensor. Only dimensions starting with `startdim` and ending
  with `enddim` are flattened. The order of elements in `t` is unchanged;
  flatten : int -> int -> tensor -> tensor := ###flatten###;

  @doc `flatten t` flattens `t` by reshaping it into a one-dimensional tensor;
  flattenAll : tensor -> tensor := ###flattenAll###;

  @doc `softShrink lambda t` applies the soft shrinkage function elementwise;
  softShrink : double -> tensor -> tensor := ###softShrink###;

  @doc `stack i ts` takes an array of tensors `ts` and appends them along the
  dimension `i` in a new tensor. All tensors need to be of the same size;
  stack : int -> array of tensor -> tensor := ###stack###;

  @doc `unsqueeze dim t` returns a new tensor with a dimension of size one
  inserted at the specified position. The returned tensor shares the same
  underlying data with `t`. A `dim` value within the range `[(dim t) - 1, (dim t) + 1)]`
  can be used. Negative `dim` will correspond to unsqueeze applied at
  `dim = dim + (dim t) + 1`;
  unsqueeze : int -> tensor -> tensor := ###unsqueeze###;

  @doc `split size dim t` splits `t` into chunks of given `size` if possible;
  split : int -> int -> tensor -> array of tensor := ###split###;

  @doc `chainMatmul ts` returns the matrix product of the NN 2-D tensors `ts`.
  This product is efficiently computed using the matrix chain order algorithm
  which selects the order in which incurs the lowest cost in terms of arithmetic
  operations. Note that since this is a function to compute the product, NN needs
  to be greater than or equal to 2. If equal to 2 then a trivial matrix-matrix
  product is returned. If NN is 1, then this is a no-op - the original matrix is
  returned as is;
  chainMatmul : array of tensor -> tensor := ###chainMatmul###;

  @doc Applies element-wise the function `GELU(x) = x * φ(x)` where `φ(x)` is
  the Cumulative Distribution Function for Gaussian Distribution;
  gelu : tensor -> tensor := ###gelu###;

  @doc `glu dim t` is the gated linear unit. Computes: `GLU(a, b) = a ⊗ Σ(b)`,
  where `t` is split in half along `dim` to form `a` and `b`, `Σ` is the sigmoid
  function and `⊗` is the element-wise product between matrices;
  glu : int -> tensor -> tensor := ###gelu###;

  @doc `view size t` returns a new tensor with the same data as the `t` but of a
  different shape according to desired `size`;
  view : array of int -> tensor -> tensor := ###view###;

  @doc `repeat times t` repeats `t` according to the number of `times` to repeat
  `t` along each dimension;
  repeat : array of int -> tensor -> tensor := ###repeat###;

  @doc `roll t shift dim` rolls the tensor `t` along the given dimension `dim`.
  Elements that are shifted beyond the last position are re-introduced at the first
  position. `shift` specifies the number of places by which the elements of the
  tensor are shifted;
  roll : tensor -> int -> int -> tensor := ###roll###;

|]
  where
    -- Unfortunately the Inferno QQ parser can't handle overloaded record dots,
    -- so the next best thing is to just use wildcards scoped to this function
    MkModelFuns{..} = mk.models
    MkDeviceFuns{..} = mk.devices
    MkFactoryFuns{..} = mk.factories
    MkConversionFuns{..} = mk.conversions
    MkFunctionalFuns{..} = mk.functional
    MkPropertyFuns{..} = mk.properties

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
          , addScalar = unbound
          , subScalar = unbound
          , mulScalar = unbound
          , divScalar = unbound
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
          , sign = unbound
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
          , glu = unbound
          , view = unbound
          , repeat = unbound
          , roll = unbound
          }
    , properties =
        MkPropertyFuns
          { numel = unbound
          , size = unbound
          , shape = unbound
          , dim = unbound
          , dtype = unbound
          , device = unbound
          , quantile = unbound
          , dquantile = unbound
          }
    }
  where
    unbound :: a
    unbound = error "Primitive is unbound"

-- These are types that implement `scalar` typeclass in Inferno
type ScalarValue = Either3 Int Double Bool
