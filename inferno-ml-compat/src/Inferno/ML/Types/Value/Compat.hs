{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Inferno.ML.Types.Value.Compat where

import Inferno.Types.Syntax (CustomType)
import Inferno.Utils.QQ.Module (moduleQuoter)
import Language.Haskell.TH.Quote (QuasiQuoter)

-- | Compatibility type for Inferno ML projects. This is intended to avoid
-- forcing a dependency on the @hasktorch@ package. For example, dummy types
-- can be provided (given correct class implementations) for tensors, models,
-- etc... for type-checking purposes purely
data MlValue tensor model mname x
  = VTensor tensor
  | VModel model
  | VModelName mname
  | VExtended x

customTypes :: [CustomType]
customTypes =
  [ "tensor"
  , -- NOTE It seems that `modelName` needs to come before `model`,
    -- otherwise Inferno's parser fails??
    "modelName"
  , "model"
  , "write"
  , "json"
  ]

mlQuoter :: QuasiQuoter
mlQuoter = moduleQuoter customTypes
