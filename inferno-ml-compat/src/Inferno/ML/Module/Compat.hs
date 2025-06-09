{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Inferno.ML.Module.Compat where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Inferno.ML.Types.Value.Compat (MlValue, mlQuoter)
import Inferno.Module.Prelude (ModuleMap)
import Inferno.Types.Value (ImplEnvM, Value)
import Prettyprinter (Pretty)

-- | Record-of-functions to generate the ML prelude, given implementations of
-- each primitive within the sub-records. This is broken up into smaller internal
-- records to avoid a huge single one
data MkModule m tensor model mname x = MkModule
  { models :: MkModelFuns m tensor model mname x
  , devices :: MkDeviceFuns m tensor model mname x
  , factories :: MkFactoryFuns m tensor model mname x
  , conversions :: MkConversionFuns m tensor model mname x
  , functional :: MkFunctionalFuns tensor
  }

-- | Primitives related to models (Torchscript modules)
--
-- NOTE: For documentation of each primitive, see the Inferno module
data MkModelFuns m tensor model mname x = MkModelFuns
  { loadModel :: Value (MlValue tensor model mname x) m
  , unsafeLoadScript :: Text -> model
  }

{- HLINT ignore "Use newtype instead of data" -}

-- | Device-related primitives, e.g. moving tensors
--
-- NOTE: For documentation of each primitive, see the Inferno module
data MkDeviceFuns m tensor model mname x = MkDeviceFuns
  { toDevice ::
      Value
        (MlValue tensor model mname x)
        (ImplEnvM m (MlValue tensor model mname x))
  -- ^ @toDevice@ is effectful, i.e. requires catching IO exceptions, so
  -- needs to be lifted into @ImplEnvM@
  }

-- | Tensor factory primitives
--
-- NOTE: For documentation of each primitive, see the Inferno module
data MkFactoryFuns m tensor model mname x = MkFactoryFuns
  { zeros :: Value (MlValue tensor model mname x) m
  , ones :: Value (MlValue tensor model mname x) m
  , randnIO :: Value (MlValue tensor model mname x) m
  }

-- | Primitives to convert to and from tensors
--
-- NOTE: For documentation of each primitive, see the Inferno module
data MkConversionFuns m tensor model mname x = MkConversionFuns
  { asTensor0 :: Value (MlValue tensor model mname x) m
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

-- | Tensor operation primitives, corresponding to those in @Torch.Functional@
--
-- NOTE: For documentation of each primitive, see the Inferno module
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
  , matMul :: tensor -> tensor -> tensor
  , mseLoss :: tensor -> tensor -> tensor
  }

mkMlModule ::
  forall m tensor model mname x.
  ( MonadThrow m
  , MonadIO m
  , Show tensor
  , Show model
  , Show mname
  , Pretty tensor
  , Pretty model
  , Pretty mname
  , Pretty x
  ) =>
  MkModule m tensor model mname x ->
  ModuleMap m (MlValue tensor model mname x)
mkMlModule _mk =
  [mlQuoter|

module ML

  enum dtype := #int | #float | #double;

  enum device := #cpu | #cuda;

|]

-- | Given concrete types, this will create a 'MkPrelude' that is suitable for
-- type-checking purposes, but which will throw an error if evaluated. This is
-- useful when creating a prelude that does not depend directly on Hasktorch
--
-- See the @inferno-ml@ package\'s prelude for a true implementation
mkUnboundPrelude :: MkModule m tensor model mname x
mkUnboundPrelude =
  MkModule
    { models =
        MkModelFuns
          { loadModel = unbound
          , unsafeLoadScript = unbound
          }
    , devices =
        MkDeviceFuns
          { toDevice = unbound
          }
    , factories =
        MkFactoryFuns
          { zeros = unbound
          , ones = unbound
          , randnIO = unbound
          }
    , conversions =
        MkConversionFuns
          { asTensor0 = unbound
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
          , matMul = unbound
          , mseLoss = unbound
          }
    }
  where
    unbound :: a
    unbound = error "Primitive is unbound"
