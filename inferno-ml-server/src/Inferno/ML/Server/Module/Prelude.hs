{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Module.Prelude
  ( bridgeModules,
    mkServerBridgePrelude,
    serverMlPrelude,
    mkPrintModules,
  )
where

import Control.Exception (ErrorCall)
import Control.Monad.Catch (MonadCatch, MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (foldrM)
import Data.Int (Int64)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Sequence ((|>))
import Data.Text (Text)
import Data.Tuple.Extra ((&&&))
import Foreign.C (CTime (CTime))
import Inferno.Eval.Error (EvalError (RuntimeError))
import Inferno.ML.Module.Prelude (getDevice, mkMlPrelude)
import Inferno.ML.Server.Module.Types
import Inferno.ML.Server.Types
  ( IValue,
    RemoteM,
    TraceWarn (CouldntMoveTensor),
    logWarn,
  )
import Inferno.ML.Types.Value (MlValue (VExtended), mlQuoter)
import Inferno.Module.Cast
import Inferno.Module.Prelude (ModuleMap)
import qualified Inferno.Types.Module
import Inferno.Types.Syntax (ExtIdent (ExtIdent))
import Inferno.Types.Value
  ( ImplEnvM,
    Value (VArray, VCustom, VEmpty, VEnum, VEpochTime, VFun, VText, VTuple),
    liftImplEnvM,
  )
import Inferno.Types.VersionControl (VCObjectHash)
import Lens.Micro.Platform
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, Pretty)
import Prettyprinter.Render.Text (renderStrict)
import System.Posix.Types (EpochTime)
import Torch (Device, Tensor)
import qualified Torch (toDevice)
import UnliftIO.Exception (handle)
import UnliftIO.IORef (atomicModifyIORef')

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
  makeWrites : forall 'a. series of 'a -> array of ('a, time) -> write := ###!makeWriteFun###;

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
            ImplEnvM m BridgeMlValue [(IValue, EpochTime)]
          extractPairs = flip foldrM mempty $ \v acc -> (: acc) <$> extractPair v

          extractPair ::
            Value c n ->
            ImplEnvM m BridgeMlValue (IValue, EpochTime)
          extractPair = \case
            VTuple [x, VEpochTime t] -> (,t) <$> toIValue x
            _ -> throwM $ RuntimeError "extractPair: expected a tuple ('a, time)"

-- | Make a prelude that works without @RemoteM@ monad
mkServerBridgePrelude ::
  forall m.
  ( MonadIO m
  , MonadCatch m
  , MonadThrow m
  ) =>
  BridgeFuns m ->
  ModuleMap m BridgeMlValue ->
  ModuleMap m BridgeMlValue
mkServerBridgePrelude bfuns mlPrelude =
  case modules & view (at "Base") &&& view (at "DataSource") of
    (Just base, Just source) ->
      modules
        & at "DataSource" .~ Nothing
        & at "Base"
          ?~ ( base
                & #moduleOpsTable
                  %~ flip (IntMap.unionWith (<>)) source.moduleOpsTable
                & #moduleTypeClasses
                  <>~ source.moduleTypeClasses
                & #moduleObjects . _1
                  <>~ view (#moduleObjects . _1) source
                & #moduleObjects . _2
                  <>~ view (#moduleObjects . _2) source
                & #moduleObjects . _3
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
      Map.unionWith (error "Redefined builtins") mlPrelude $
        bridgeModules @_ bfuns

-- | ML prelude for use only in @RemoteM@ (needed for tracing effects)
serverMlPrelude :: ModuleMap RemoteM (MlValue BridgeValue)
serverMlPrelude =
  -- NOTE There's no risk of overlap in module names here, so we can just
  -- use `union` instead of `unionWith`
  Map.union printModules $ mkMlPrelude toDeviceFun
  where
    toDeviceFun :: BridgeV RemoteM
    toDeviceFun =
      VFun $ \case
        VEnum _ e ->
          -- The device that the user wants to move the tensor to; currently
          -- can only be `#cpu` or `#cuda`
          getDevice e <&> \device ->
            -- Original tensor to be moved (as Inferno value)
            VFun $ \vtensor -> do
              fromValue vtensor >>= liftIO . toDeviceIO device >>= \case
                -- This is the (potentially) moved tensor; its having been moved
                -- or not depends on the devices involved
                --
                -- In most cases, this would happen if a CPU-only `inferno-ml-server`
                -- tries to evaluate `ML.toDevice t #cuda`; with no CUDA device
                -- it can't be moved. Logging is more important, however, if
                -- for some reason CUDA-enabled `inferno-ml-server`s can't move
                -- the device when they should be able to. Logging can at least
                -- help with detecting these types of problems
                --
                -- It's better to _not_ throw an exception in the `Left` case
                -- here so the same scripts can be used without modification
                -- on both CPU and GPU `inferno-ml-server`s
                Right tensor -> pure $ toValue @_ @_ @Tensor tensor
                Left tensor -> do
                  liftImplEnvM . logWarn . CouldntMoveTensor $ show device
                  pure $ toValue @_ @_ @Tensor tensor
        _ -> throwM $ RuntimeError "toDeviceFun: expecting a device enum"

    printModules :: ModuleMap RemoteM (MlValue BridgeValue)
    printModules = mkPrintModules printFun printWithFun
      where
        -- Sticks the prettified value onto the end of the "console" output
        printFun :: BridgeV RemoteM
        printFun = VFun $ writeConsole . renderValue

        -- Sticks the prettified value onto the end of the "console" output,
        -- along with its text prefix
        printWithFun :: BridgeV RemoteM
        printWithFun = VFun $ \case
          VText t -> pure . VFun $ writeConsole . ((t <>) . (" " <>)) . renderValue
          _ -> throwM $ RuntimeError "printWith: expecting a text value"

        writeConsole :: Text -> BridgeImplM RemoteM
        writeConsole t =
          liftImplEnvM $
            fmap toValue $
              (`atomicModifyIORef'` ((|> t) &&& const ())) =<< view #console


-- Workaround for `error`s in Torch's "pure" `toDevice`. If the original
-- tensor cannot be moved, `Left Tensor` is returned to signal failure to move
-- the tensor to the new device, i.e. the original tensor is returned;
-- `Right Tensor` is the successfully moved new tensor
toDeviceIO :: Device -> Tensor -> IO (Either Tensor Tensor)
toDeviceIO device t1 = handle handler $ t2 `seq` pure (Right t2)
  where
    -- If the `error` occurs, the original tensor is returned
    handler :: ErrorCall -> IO (Either Tensor Tensor)
    handler = const . pure $ Left t1

    -- The potentialy moved tensor. If the requested device is not available,
    -- i.e. moving to CUDA on a CPU-only device, this "pure" function will
    -- throw an `error`
    t2 :: Tensor
    t2 = Torch.toDevice device t1

-- | Provided with implementations for @print@ and @printWith@, creates a
-- @Print@ module with effects for printing to the \"console\"
--
-- These are defined separately from the main @inferno-ml@ modules as they
-- are not strictly related to Inferno ML and require a specific implementation
mkPrintModules ::
  forall m.
  ( MonadIO m
  , MonadCatch m
  , MonadThrow m
  ) =>
  -- | @Print.print@ implementation
  BridgeV m ->
  -- | @Print.printWith@ implementation
  BridgeV m ->
  ModuleMap m (MlValue BridgeValue)
mkPrintModules printFun printWithFun =
  [mlQuoter|
module Print

   @doc Convert a value to text and print it to the console;
   print : forall 'a. 'a -> () := ###!printFun###;

   @doc Convert a value to text and print it to the console, with a text prefix;
   printWith : forall 'a. text -> 'a := ###!printWithFun###;

   @doc Convert a value to text;
   show : forall 'a. 'a -> text := ###!showFun###;

  |]
  where
    showFun :: BridgeV m
    showFun = VFun $ pure . toValue . renderValue

renderValue :: Pretty v => Value v (ImplEnvM m v) -> Text
renderValue = renderStrict . layoutPretty defaultLayoutOptions . pretty
