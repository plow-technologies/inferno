{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Inferno.ML.Server.Client
  ( statusC,
    inferenceC,
    cancelC,
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
statusC :: ClientM ServerStatus

-- | Run an inference parameter
inferenceC ::
  -- | SQL identifier of the inference parameter to be run
  Id (InferenceParam uid gid p s) ->
  -- | Optional resolution for scripts that use e.g. @valueAt@; defaults to
  -- the param\'s stored resolution if not provided. This lets users override
  -- the resolution on an ad-hoc basis without needing to alter the stored
  -- values for the parameter
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
statusC :<|> inferenceC :<|> cancelC =
  client api

api :: Proxy (InfernoMlServerAPI uid gid p s t)
api = Proxy
