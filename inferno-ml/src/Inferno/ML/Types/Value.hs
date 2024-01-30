module Inferno.ML.Types.Value where

import qualified Data.Text as Text
import Inferno.Module.Cast (FromValue (..), ToValue (..), couldNotCast)
import Inferno.Types.Syntax (CustomType)
import Inferno.Types.Value (Value (VCustom))
import Inferno.Utils.QQ.Module (moduleQuoter)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Prettyprinter (Pretty (pretty), align)
import qualified Torch as T

data MlValue
  = VTensor T.Tensor
  | VModel T.ScriptModule

instance Eq MlValue where
  (VTensor t1) == (VTensor t2) = t1 == t2
  _ == _ = False

instance Pretty MlValue where
  pretty = \case
    VTensor t -> align (pretty $ Text.pack $ show t)
    VModel m -> align (pretty $ Text.pack $ show m)

instance ToValue MlValue m T.Tensor where
  toValue = VCustom . VTensor

instance FromValue MlValue m T.Tensor where
  fromValue (VCustom (VTensor t)) = pure t
  fromValue v = couldNotCast v

instance ToValue MlValue m T.ScriptModule where
  toValue = VCustom . VModel

instance FromValue MlValue m T.ScriptModule where
  fromValue (VCustom (VModel t)) = pure t
  fromValue v = couldNotCast v

customTypes :: [CustomType]
customTypes = ["tensor", "model"]

mlQuoter :: QuasiQuoter
mlQuoter = moduleQuoter customTypes
