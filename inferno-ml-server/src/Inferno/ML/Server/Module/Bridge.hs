{-# LANGUAGE DataKinds #-}

module Inferno.ML.Server.Module.Bridge
  ( mkBridgeFuns,
  )
where

import Control.Category ((>>>))
import Data.Int (Int64)
import Inferno.ML.Server.Types
import Inferno.Module.Cast (ToValue (toValue))
import Inferno.Types.Value
  ( ImplicitCast (ImplicitCast),
    Value (VDouble, VOne, VText, VTuple),
    liftImplEnvM,
  )
import System.Posix.Types (EpochTime)

-- | Create the functions that will be used for the Inferno primitives related
-- to the data source. Effects defined in @RemoteM@ are wrapped in @ImplEnvM m ...@
mkBridgeFuns ::
  -- | @valueAt@
  (Int64 -> PID -> EpochTime -> RemoteM IValue) ->
  -- | @latestValueAndTimeBefore@
  (EpochTime -> PID -> RemoteM IValue) ->
  -- | @valuesBetween@
  (Int64 -> PID -> EpochTime -> EpochTime -> RemoteM IValue) ->
  BridgeFuns RemoteM
mkBridgeFuns valueAt latestValueAndTimeBefore valuesBetween =
  BridgeFuns
    valueAtFun
    latestValueAndTimeBeforeFun
    latestValueAndTimeFun
    valuesBetweenFun
  where
    valueAtFun :: BridgeV RemoteM
    valueAtFun = toValue $ ImplicitCast @"resolution" inputFunction
      where
        inputFunction ::
          InverseResolution -> PID -> EpochTime -> BridgeImplM RemoteM
        inputFunction res p =
          liftImplEnvM
            . fmap toInfernoValue
            -- TODO Need to make these resolutions tidier...
            . valueAt (resolutionToInt res) p

        toInfernoValue :: IValue -> BridgeV RemoteM
        toInfernoValue =
          -- Note that only double and text values can be directly queried;
          -- if we get a value that's not `VEmpty`, we need to wrap it in a
          -- `VOne`, since this is supposed to be a `Some` on the Inferno side
          fromIValue >>> \case
            d@VDouble{} -> VOne d
            t@VText{} -> VOne t
            v -> v

    latestValueAndTimeBeforeFun :: BridgeV RemoteM
    latestValueAndTimeBeforeFun = toValue latestInputFunction
      where
        latestInputFunction :: EpochTime -> PID -> BridgeImplM RemoteM
        latestInputFunction t =
          liftImplEnvM
            . fmap toInfernoValue
            . latestValueAndTimeBefore t

        toInfernoValue :: IValue -> BridgeV RemoteM
        toInfernoValue =
          fromIValue >>> \case
            t@VTuple{} -> VOne t
            v -> v

    latestValueAndTimeFun :: BridgeV RemoteM
    latestValueAndTimeFun = toValue $ ImplicitCast @"now" latestInputFunction
      where
        latestInputFunction :: EpochTime -> PID -> BridgeImplM RemoteM
        latestInputFunction t =
          liftImplEnvM
            . fmap toInfernoValue
            . latestValueAndTimeBefore t

        toInfernoValue :: IValue -> BridgeV RemoteM
        toInfernoValue =
          fromIValue >>> \case
            t@VTuple{} -> VOne t
            v -> v

    valuesBetweenFun :: BridgeV RemoteM
    valuesBetweenFun =
      toValue $
        ImplicitCast @"resolution" valuesBetweenFunction
      where
        valuesBetweenFunction ::
          InverseResolution ->
          PID ->
          EpochTime ->
          EpochTime ->
          BridgeImplM RemoteM
        valuesBetweenFunction r pid t1 =
          liftImplEnvM
            . fmap fromIValue
            . valuesBetween (resolutionToInt r) pid t1
