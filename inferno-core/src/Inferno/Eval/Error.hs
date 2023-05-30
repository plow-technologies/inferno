module Inferno.Eval.Error where

import Control.Monad.Catch (Exception)
import Inferno.Types.Syntax (ExtIdent)

data EvalError
  = AssertionFailed
  | RuntimeError String
  | CastError String
  | NotFoundInImplicitEnv ExtIdent
  deriving (Show, Eq)

instance Exception EvalError
