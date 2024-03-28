{-# LANGUAGE DataKinds #-}

module Inferno.ML.Server.Module.Bridge
  ( mkBridgeFuns,
  )
where

import Control.Category ((>>>))
import Control.Monad.Catch (MonadThrow (throwM), handle)
import Data.Int (Int64)
import qualified Data.Text as Text
import Inferno.Eval.Error (EvalError (RuntimeError))
import Inferno.ML.Server.Types
import Inferno.Module.Cast (ToValue (toValue))
import Inferno.Types.Value
  ( ImplicitCast (ImplicitCast),
    Value (VDouble, VOne, VTuple),
    liftImplEnvM,
  )
import System.Posix.Types (EpochTime)
import Torch (Tensor)

-- | Create the functions that will be used for the Inferno primitives related
-- to the data source. Effects defined in @RemoteM@ are wrapped in @ImplEnvM m ...@
mkBridgeFuns ::
  -- | @valueAt@
  (Int64 -> PID -> EpochTime -> RemoteM IValue) ->
  -- | @latestValueAndTimeBefore@
  (EpochTime -> PID -> RemoteM IValue) ->
  -- | @writePairs@
  --
  -- FIXME `writePairs` will be removed soon
  (PID -> Tensor -> RemoteM ()) ->
  BridgeFuns RemoteM
mkBridgeFuns valueAt latestValueAndTimeBefore writePairs =
  BridgeFuns
    valueAtFun
    latestValueAndTimeBeforeFun
    latestValueAndTimeFun
    writePairsFun
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
          fromIValue >>> \case
            d@VDouble {} -> VOne d
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
            t@VTuple {} -> VOne t
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
            t@VTuple {} -> VOne t
            v -> v

    -- FIXME `writePairs` will be removed soon
    writePairsFun :: BridgeV RemoteM
    writePairsFun = toValue writePairsFunction
      where
        writePairsFunction :: PID -> Tensor -> BridgeImplM RemoteM
        writePairsFunction p =
          handle raiseRuntime
            . liftImplEnvM
            . fmap (const (VTuple mempty))
            . writePairs p

        raiseRuntime :: RemoteError -> BridgeImplM RemoteM
        raiseRuntime = \case
          -- If the tensor provided as an argument isn't the correct shape, this
          -- will be raised, so the user should be informed clearly (it will be
          -- thrown as an `InvalidScript` internally)
          InvalidScript t -> throwM . RuntimeError $ Text.unpack t
          e -> throwM e
