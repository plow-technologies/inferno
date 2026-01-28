module Inferno.ML.Server.Bridge
  ( initializeInferno,
  )
where

import Data.Int (Int64)
import Inferno.Core (Interpreter, mkInferno)
import qualified Inferno.ML.Server.Client.Bridge as Bridge
import Inferno.ML.Server.Module.Bridge (mkBridgeFuns)
import Inferno.ML.Server.Module.Prelude
  ( callBridge,
    getBridgeInfo,
    mkServerBridgePrelude,
    serverMlPrelude,
  )
import Inferno.ML.Server.Types
import Inferno.ML.Types.Value.Compat (customTypes)
import System.Posix.Types (EpochTime)

-- | Retrieve the 'BridgeInfo' associated with an inference param and update the
-- Inferno interpreter to use the given bridge (the special primitives must call
-- the bridge to read\/write data from\/to the data source)
initializeInferno ::
  Id InferenceParam -> RemoteM (Interpreter RemoteM BridgeMlValue)
initializeInferno ipid =
  (`mkInferno` customTypes)
    . (`mkServerBridgePrelude` serverMlPrelude ipid)
    . mkFuns
    =<< getBridgeInfo ipid
  where
    mkFuns :: BridgeInfo -> BridgeFuns RemoteM
    mkFuns bi = mkBridgeFuns valueAt latestValueAndTimeBefore valuesBetween
      where
        valueAt :: Int64 -> PID -> EpochTime -> RemoteM IValue
        valueAt res pid = callBridge ipid bi . Bridge.valueAtC res pid

        latestValueAndTimeBefore :: EpochTime -> PID -> RemoteM IValue
        latestValueAndTimeBefore t =
          callBridge ipid bi . Bridge.latestValueAndTimeBeforeC t

        valuesBetween :: Int64 -> PID -> EpochTime -> EpochTime -> RemoteM IValue
        valuesBetween res pid t1 =
          callBridge ipid bi . Bridge.valuesBetweenC res pid t1
