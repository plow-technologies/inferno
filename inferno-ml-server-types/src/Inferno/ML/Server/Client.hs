{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Client
  ( healthC,
    inferenceC,
  )
where

import Conduit (ConduitT)
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (Scientific)
import Inferno.ML.Server.Types
import Servant ((:<|>) ((:<|>)))
import Servant.Client.Streaming (ClientM, client)

healthC :: ClientM ()
inferenceC ::
  forall uid gid.
  InferenceRequest uid gid ->
  ClientM (ConduitT () (AsValue Scientific) IO ())
healthC :<|> inferenceC = client api

api :: Proxy (InfernoMlServerAPI uid gid)
api = Proxy
