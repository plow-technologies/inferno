module Inferno.Eval.Error where

import Inferno.Types.Syntax (ExtIdent)

data EvalError
  = AssertionFailed
  | RuntimeError String
  | CastError String
  | NotFoundInImplicitEnv ExtIdent
  deriving (Show, Eq)
