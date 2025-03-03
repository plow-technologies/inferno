{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Module.Prelude
  ( bridgeModules,
    mkBridgePrelude,
  )
where

import Control.Monad.Catch (MonadCatch, MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (foldrM)
import Data.Int (Int64)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Tuple.Extra ((&&&))
import Foreign.C (CTime (CTime))
import Inferno.Eval.Error (EvalError (RuntimeError))
import Inferno.ML.Module.Prelude (mlPrelude)
import Inferno.ML.Server.Module.Types
import Inferno.ML.Server.Types (IValue)
import Inferno.ML.Types.Value (MlValue (VExtended), mlQuoter)
import Inferno.Module.Cast
import Inferno.Module.Prelude (ModuleMap)
import Inferno.Types.Syntax (ExtIdent (ExtIdent))
import Inferno.Types.Value
  ( ImplEnvM,
    Value (VArray, VCustom, VEmpty, VEpochTime, VFun, VTuple),
  )
import Inferno.Types.VersionControl (VCObjectHash)
import Lens.Micro.Platform
import System.Posix.Types (EpochTime)

-- | Contains primitives for use in bridge prelude, including those to read\/write
-- data
--
-- NOTE: These functions are required for our use of @inferno-ml-server@ elsewhere;
-- open-source users will presumably not find these useful. Unfortunately, the
-- Inferno interpreter used by the server needs to be initialized with these
-- primitives
bridgeModules ::
  forall m.
  (MonadThrow m, MonadIO m) =>
  BridgeFuns m ->
  ModuleMap m BridgeMlValue
bridgeModules
  ( BridgeFuns
      valueAt
      latestValueAndTimeBefore
      latestValueAndTime
      valuesBetween
    ) =
    [mlQuoter|
module DataSource
  @doc Create a `write` object encapsulating an array of `(time, 'a)` values to be
  written to a given parameter. All ML scripts must return an array of such `write`
  objects, potentially empty, and this is the only way for them to write values to parameters.;
  makeWrites : forall 'a. series of 'a -> array of (time, 'a) -> write := ###!makeWriteFun###;

  toResolution : int -> resolution := ###toResolution###;

  resolutionToInt : resolution -> int := ###resolutionToInt###;

  @doc Returns the value of the parameter in the chunk containing the given time, if it exists.
  For example, if resolution is 128 (thus chunks are 0-127, 128-255, 256-383, ...),
  and the parameter `p` has values:
  ~~~
  V:       1.0        |           |        3.0        |
  T: 0 ... 120 ... 127 128 ... 255 256 ... 300 ... 383 384 ...
  ~~~
  then:
  ~~~inferno
  open Time in
  valueAt p (toTime (seconds 128)) == Some 1.0
  valueAt p (toTime (seconds 129)) == None
  valueAt p (toTime (seconds 380)) == Some 3.0
  ~~~;
  valueAt :
    forall 'a. {implicit resolution : resolution}
    => series of 'a
    -> time
    -> option of 'a
    := ###!valueAt###;

  latestValueAndTimeBefore :
    forall 'a
    . time
    -> series of 'a
    -> option of ('a, time)
    := ###!latestValueAndTimeBefore###;

  @doc Returns the latest value of the parameter before the given time, if one exists.;
  latestValueBefore : forall 'a. time -> series of 'a -> option of 'a :=
    fun t p -> match latestValueAndTimeBefore t p with {
      | Some (x, _) -> Some x
      | None -> None
    };

  @doc Returns the latest value of the parameter (and the corresponding timestamp),
  if one exists.;
  latestValueAndTime :
    forall 'a
    . { implicit now : time }
    => series of 'a
    -> option of ('a, time)
    := ###!latestValueAndTime###;

  @doc Returns the latest value of the parameter, if one exists.;
  latestValue :
    forall 'a . { implicit now : time } => series of 'a -> option of 'a :=
      fun p -> match latestValueAndTime p with {
        | Some (x, _) -> Some x
        | None -> None
      };

  @doc Returns the value of the parameter in the chunk containing the given time,
  if it exists, or in the closest adjacent chunk (previous or next chunk), if it exists.
  For example, if resolution is 128 (thus chunks are 0-127, 128-255, 256-383, ...),
  and the parameter `p` has values:
  ~~~
  V:       1.0        |           |        3.0        |4.0
  T: 0 ... 120 ... 127 128 ... 255 256 ... 300 ... 383 384 ...
  ~~~
  then:
  ~~~inferno
  open Time in
  valueAtOrAdjacent p (toTime (seconds 380)) == Some 3.0  // value from current chunk
  valueAtOrAdjacent p (toTime (seconds 130)) == Some 1.0  // value from previous chunk
  valueAtOrAdjacent p (toTime (seconds 200)) == Some 3.0  // value from next chunk
  valueAtOrAdjacent p (toTime (seconds 1024)) == None
  ~~~;
  valueAtOrAdjacent :
    forall 'a. { implicit resolution : resolution }
    => series of 'a
    -> time
    -> option of 'a
    := ###!valueAtOrAdjacent###;
  @doc Returns all values between two times, using the implicit resolution.

  If the resolution is set to 1, this returns all the events (actual values, not approximations) in the given time window.;
  valuesBetween : forall 'a. { implicit resolution : resolution }
    => series of 'a -> time -> time -> array of ('a, time) := ###!valuesBetween###;
|]
    where
      valueAtOrAdjacent :: BridgeV m
      valueAtOrAdjacent = VFun $ \vpid -> pure . VFun $ \case
        vt@(VEpochTime (CTime t)) ->
          applyValueAtFun vpid vt >>= \case
            VEmpty -> do
              getAdjacent t (-) >>= applyValueAtFun vpid >>= \case
                VEmpty -> getAdjacent t (+) >>= applyValueAtFun vpid
                v -> pure v
            v -> pure v
        _ -> throwM $ RuntimeError "valueAtOrAdjacent: unexpected input type"
        where
          getAdjacent ::
            Int64 ->
            (Int64 -> Int64 -> Int64) ->
            ImplEnvM m BridgeMlValue (BridgeV m)
          getAdjacent t f =
            (view . at . ExtIdent . Right) "resolution"
              >>= maybe
                (throwM (RuntimeError "No resolution in implicit env"))
                pure
              >>= \case
                VCustom (VExtended (VResolution ir)) ->
                  pure $
                    VEpochTime . CTime . fromIntegral $
                      fromIntegral t `f` resolutionToInt ir `div` 2
                _ -> throwM $ RuntimeError "Unexpected resolution type"

      applyValueAtFun ::
        BridgeV m -> BridgeV m -> ImplEnvM m BridgeMlValue (BridgeV m)
      applyValueAtFun vpid vt = case valueAt of
        VFun f1 ->
          f1 vpid >>= \case
            VFun f2 -> f2 vt
            _ -> throwM e
        _ -> throwM e
        where
          e :: EvalError
          e = RuntimeError "valueAt: expected a function"

      makeWriteFun :: BridgeV m
      makeWriteFun =
        VFun $ \case
          VCustom (VExtended (VSeries pid)) ->
            pure . VFun $ \case
              VArray vs ->
                VCustom . VExtended . VWrite . (pid,) <$> extractPairs vs
              _ -> throwM $ RuntimeError "makeWrite: expecting an array"
          _ -> throwM $ RuntimeError "makeWrite: expecting a pid"
        where
          extractPairs ::
            [Value c n] ->
            ImplEnvM m BridgeMlValue [(EpochTime, IValue)]
          extractPairs = flip foldrM mempty $ \v acc -> (: acc) <$> extractPair v

          extractPair ::
            Value c n ->
            ImplEnvM m BridgeMlValue (EpochTime, IValue)
          extractPair = \case
            VTuple [VEpochTime t, x] -> (t,) <$> toIValue x
            _ -> throwM $ RuntimeError "extractPair: expected a tuple (time, 'a)"

mkBridgePrelude ::
  forall m.
  ( MonadIO m
  , MonadThrow m
  , MonadCatch m
  ) =>
  BridgeFuns m ->
  ModuleMap m BridgeMlValue
mkBridgePrelude bfuns =
  case modules & view (at "Base") &&& view (at "DataSource") of
    (Just base, Just source) ->
      modules
        & at "DataSource"
        .~ Nothing
        & at "Base"
        ?~ ( base
              & #moduleOpsTable
              %~ flip
                (IntMap.unionWith (<>))
                (view #moduleOpsTable source)
              & #moduleTypeClasses
              <>~ view #moduleTypeClasses source
              & #moduleObjects
              . _1
              <>~ view (#moduleObjects . _1) source
              & #moduleObjects
              . _2
              <>~ view (#moduleObjects . _2) source
              & #moduleObjects
              . _3
              %~ (`combineTermEnv` view (#moduleObjects . _3) source)
           )
    _ -> error "mkBridgePrelude: Missing Base and/or DataSource modules"
  where
    combineTermEnv ::
      (Map.Map ExtIdent v, Map.Map VCObjectHash e) ->
      (Map.Map ExtIdent v, Map.Map VCObjectHash e) ->
      (Map.Map ExtIdent v, Map.Map VCObjectHash e)
    combineTermEnv trm1 trm2 = trm1 >>= \x -> trm2 <&> (x <>)

    modules :: ModuleMap m BridgeMlValue
    modules =
      Map.unionWith
        (error "Redefined builtins")
        (mlPrelude @_ @BridgeValue)
        $ bridgeModules @_ bfuns
