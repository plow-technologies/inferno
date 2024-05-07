{-# LANGUAGE RankNTypes #-}

module Inferno.ML.Server.Bridge
  ( registerBridgeInfo,
    getBridgeInfo,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (encodeFile)
import Data.Int (Int64)
import Inferno.Core (mkInferno)
import Inferno.ML.Server.Module.Bridge (mkBridgeFuns)
import Inferno.ML.Server.Module.Prelude (mkBridgePrelude)
import Inferno.ML.Server.Types
import Inferno.ML.Server.Utils
import Inferno.ML.Types.Value (customTypes)
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
import UnliftIO.IORef (atomicWriteIORef, readIORef)

-- | Save the provided 'BridgeInfo' and update the Inferno interpreter to use
-- the given bridge (the special primitives must call the bridge to read\/write
-- data from\/to the data source)
registerBridgeInfo :: BridgeInfo -> RemoteM ()
registerBridgeInfo bi = do
  logTrace $ RegisteringBridge bi
  liftIO $ encodeFile bridgeCache bi
  (`atomicWriteIORef` Just bi) =<< view (#bridge . #info)
  interpreter <- mkInferno @_ @BridgeMlValue (mkBridgePrelude funs) customTypes
  (`atomicWriteIORef` Just interpreter)
    =<< view #interpreter
  where
    funs :: BridgeFuns RemoteM
    funs = mkBridgeFuns valueAt latestValueAndTimeBefore valuesBetween

    valueAt :: Int64 -> PID -> EpochTime -> RemoteM IValue
    valueAt res pid t = callBridge =<< getBridgeRoute #valueAt ?? res ?? pid ?? t

    latestValueAndTimeBefore :: EpochTime -> PID -> RemoteM IValue
    latestValueAndTimeBefore t pid =
      callBridge =<< getBridgeRoute #latestValueAndTimeBefore ?? t ?? pid

    valuesBetween :: Int64 -> PID -> EpochTime -> EpochTime -> RemoteM IValue
    valuesBetween res pid t1 t2 = callBridge =<< getBridgeRoute #valuesBetween ?? res ?? pid ?? t1 ?? t2

-- | Get the previously saved 'BridgeInfo', if any
getBridgeInfo :: RemoteM (Maybe BridgeInfo)
getBridgeInfo = readIORef =<< view (#bridge . #info)

-- | Call one of the bridge endpoints using the saved 'BridgeInfo', throwing an
-- exception if the info has not been saved yet
callBridge :: NFData a => ClientM a -> RemoteM a
callBridge c =
  view (#bridge . #info) >>= readIORef >>= \case
    Nothing -> throwM BridgeNotRegistered
    Just bi ->
      either (throwM . ClientError) pure
        =<< liftIO . runClientM c
        =<< mkEnv
      where
        mkEnv :: RemoteM ClientEnv
        mkEnv = asks $ (`mkClientEnv` url) . view #manager

        url :: BaseUrl
        url =
          BaseUrl
            Http
            (view (#host . to show) bi)
            (view (#port . to fromIntegral) bi)
            mempty

getBridgeRoute :: Lens' BridgeClient (a -> b) -> RemoteM (a -> b)
getBridgeRoute l = view $ #bridge . #client . l
