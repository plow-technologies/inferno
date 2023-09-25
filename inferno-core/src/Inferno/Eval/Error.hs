module Inferno.Eval.Error where

import Control.DeepSeq (NFData, rnf)
import Control.Monad.Catch (Exception)
import Inferno.Types.Syntax (ExtIdent)

data EvalError
  = AssertionFailed
  | RuntimeError String
  | CastError String
  | NotFoundInImplicitEnv ExtIdent
  deriving (Show, Eq)

instance Exception EvalError

instance NFData EvalError where
  rnf AssertionFailed = ()
  rnf (RuntimeError x) = rnf x
  rnf (CastError x) = rnf x
  rnf (NotFoundInImplicitEnv x) = rnf x
