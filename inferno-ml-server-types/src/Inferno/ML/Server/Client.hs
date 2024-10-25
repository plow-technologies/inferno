{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Client
  ( statusC,
    inferenceC,
    inferenceTestC,
    cancelC,
  )
where

import Data.Aeson (ToJSON)
import Data.Int (Int64)
import Data.Proxy (Proxy (Proxy))
import Data.UUID (UUID)
import Inferno.ML.Server.Types
import Servant.Client.Streaming (ClientM, client)

-- | Get the status of the server. @Nothing@ indicates that an inference job
-- is being evaluated. @Just ()@ means the server is idle
statusC :: ClientM ServerStatus
statusC = client $ Proxy @StatusAPI

-- | Cancel the existing inference job, if it exists
cancelC :: ClientM ()
cancelC = client $ Proxy @CancelAPI

-- | Run an inference parameter
inferenceC ::
  forall gid p .
  -- | SQL identifier of the inference parameter to be run
  Id (InferenceParam gid p ) ->
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
inferenceC = client $ Proxy @(InferenceAPI gid p)

-- | Run an inference parameter
inferenceTestC ::
  forall gid p .
  ToJSON p =>
  -- | SQL identifier of the inference parameter to be run
  Id (InferenceParam gid p ) ->
  Maybe Int64 ->
  UUID ->
  EvaluationEnv gid p ->
  ClientM (WriteStream IO)
inferenceTestC = client $ Proxy @(InferenceTestAPI gid p)
