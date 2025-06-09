{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Inferno.ML.Server.Bridge
  ( initializeInferno,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Int (Int64)
import Database.PostgreSQL.Simple (Only (Only), Query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Inferno.Core (Interpreter, mkInferno)
import qualified Inferno.ML.Server.Client.Bridge as Bridge
import Inferno.ML.Server.Module.Bridge (mkBridgeFuns)
import Inferno.ML.Server.Module.Prelude (mkServerBridgePrelude, serverMlPrelude)
import Inferno.ML.Server.Types
import Inferno.ML.Server.Utils
import Inferno.ML.Types.Value.Compat (customTypes)
import Lens.Micro.Platform
import Servant.Client.Streaming
  ( BaseUrl (BaseUrl),
    ClientEnv,
    ClientM,
    Scheme (Http),
    mkClientEnv,
    runClientM,
  )
import System.Posix.Types (EpochTime)

-- | Retrieve the 'BridgeInfo' associated with an inference param and update the
-- Inferno interpreter to use the given bridge (the special primitives must call
-- the bridge to read\/write data from\/to the data source)
initializeInferno ::
  Id InferenceParam -> RemoteM (Interpreter RemoteM BridgeMlValue)
initializeInferno ipid = do
  (`mkInferno` customTypes) . (`mkServerBridgePrelude` serverMlPrelude) . mkFuns
    =<< getBridgeInfo
  where
    -- There should always be a bridge saved for the param
    getBridgeInfo :: RemoteM BridgeInfo
    getBridgeInfo = firstOrThrow (NoBridgeSaved ipid) =<< queryStore q (Only ipid)
      where
        q :: Query
        q = [sql| SELECT * FROM bridges WHERE id = ? |]

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

-- | Call one of the bridge endpoints using the given 'BridgeInfo'
callBridge :: (NFData a) => Id InferenceParam -> BridgeInfo -> ClientM a -> RemoteM a
callBridge ipid bi c =
  either (throwRemoteError . ClientError ipid . show) pure =<< liftIO . runClientM c =<< mkEnv
  where
    mkEnv :: RemoteM ClientEnv
    mkEnv = asks $ (`mkClientEnv` url) . view #manager
      where
        url :: BaseUrl
        url = BaseUrl Http (show bi.host) (fromIntegral bi.port) mempty
