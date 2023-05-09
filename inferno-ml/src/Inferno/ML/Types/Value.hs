module Inferno.ML.Types.Value where

import qualified Data.Text as Text
import Inferno.Module.Cast (FromValue (..), ToValue (..), couldNotCast)
import Inferno.Types.Value (Value (VCustom))
import Prettyprinter (Pretty (pretty), align)
import qualified Torch as T

data MlValue
  = VTensor T.Tensor
  | VCustom2 -- TODO is this the only way we can extend to both MlValue and PlowValue?

instance Eq MlValue where
  (VTensor t1) == (VTensor t2) = t1 == t2
  _ == _ = False

instance Pretty MlValue where
  pretty = \case
    VTensor t -> align (pretty $ Text.pack $ show t)
    VCustom2 -> "custom"

instance ToValue MlValue m T.Tensor where
  toValue = pure . VCustom . VTensor

instance Pretty MlValue => FromValue MlValue m T.Tensor where
  fromValue (VCustom (VTensor t)) = pure t
  fromValue v = couldNotCast v
