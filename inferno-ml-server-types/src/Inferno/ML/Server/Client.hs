{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Inferno.ML.Server.Client
  ( statusC,
    inferenceC,
    cancelC,
    registerBridgeC,
    checkBridgeC,
  )
where

import Data.Int (Int64)
import Data.Proxy (Proxy (Proxy))
import Data.UUID (UUID)
import Inferno.ML.Server.Types
import Servant ((:<|>) ((:<|>)))
import Servant.Client.Streaming (ClientM, client)

-- | Get the status of the server. @Nothing@ indicates that an inference job
-- is being evaluated. @Just ()@ means the server is idle
statusC :: ClientM (Maybe ())

-- | Run an inference parameter
inferenceC ::
  -- | SQL identifier of the inference parameter to be run
  Id (InferenceParam uid gid p s) ->
  -- | Optional resolution for scripts that use e.g. @valueAt@; defaults to
  -- 128 if not specified
  Maybe Int64 ->
  -- | Job identifer. This is used to save execution statistics for each
  -- inference evaluation
  UUID ->
  -- | Note that every item in the output stream (first element of each
  -- outer tuple) should be declared as writable by the corresponding
  -- inference parameter. It is the responsibility of the runtime system
  -- (not defined in this repository) to verify this before directing
  -- the writes to their final destination
  ClientM (WriteStream IO)

-- | Cancel the existing inference job, if it exists
cancelC :: ClientM ()

-- | Register the information required to communicate with the bridge server
registerBridgeC :: BridgeInfo -> ClientM ()

-- | Check if any bridge information has been previously registered with this
-- server instance
checkBridgeC :: ClientM (Maybe BridgeInfo)
statusC
  :<|> inferenceC
  :<|> cancelC
  :<|> registerBridgeC
  :<|> checkBridgeC =
    client api

api :: Proxy (InfernoMlServerAPI uid gid p s t)
api = Proxy
