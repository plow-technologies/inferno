{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.ML.Server.Module.Prelude
  ( bridgeModules,
    mkServerBridgePrelude,
    serverMlPrelude,
    mkPrintModules,
    getBridgeInfo,
    callBridge,
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (ErrorCall)
import Control.Monad.Catch (MonadCatch, MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Data.Foldable (foldrM)
import Data.Int (Int64)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Sequence ((|>))
import Data.Text (Text)
import Data.Tuple.Extra ((&&&))
import Database.PostgreSQL.Simple (Only (Only), Query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Foreign.C (CTime (CTime))
import Inferno.Eval.Error (EvalError (RuntimeError))
import Inferno.ML.Module.Prelude
  ( MlModule,
    defaultMlModule,
    getDevice,
    mkMlPrelude,
  )
import Inferno.ML.Server.Inference.Model (loadModel)
import Inferno.ML.Server.Module.Types
import Inferno.ML.Server.Types
import Inferno.ML.Server.Utils
import Inferno.ML.Types.Value
  ( MlValue,
    ModelName (ModelName),
    pattern Bedrock,
    pattern TorchScript,
    pattern VExtended,
    pattern VModel,
    pattern VModelName,
  )
import Inferno.ML.Types.Value.Compat (mlQuoter)
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
import Prettyprinter (Pretty, defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)
import Servant.Client.Streaming
  ( BaseUrl (BaseUrl),
    ClientEnv,
    ClientM,
    Scheme (Http),
    mkClientEnv,
    runClientM,
  )
import System.Posix.Types (EpochTime)
import Torch (Device, Tensor)
import qualified Torch (toDevice)
import UnliftIO.Exception
  ( displayException,
    evaluate,
    fromException,
    tryAny,
  )
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
  @doc Create a `write` object encapsulating an array of `('a, time)` values to be
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
serverMlPrelude ::
  -- | Used for communicating with bridge when @prompt@ is evaluated
  Id InferenceParam ->
  ModuleMap RemoteM (MlValue BridgeValue)
serverMlPrelude ipid =
  -- NOTE There's no risk of overlap in module names here, so we can just
  -- use `union` instead of `unionWith`
  Map.union printModules $ mkMlPrelude mlModule
  where
    mlModule :: MlModule (ImplEnvM RemoteM (MlValue BridgeValue)) BridgeValue
    mlModule =
      -- Note that the type app seems to be necessary for inference to work,
      -- even though `mlModule` has a type signature above
      defaultMlModule @(ImplEnvM RemoteM (MlValue BridgeValue)) @BridgeValue
        -- Overrides the default, less-safe `toDevice` implementation with one
        -- that checks if the tensor has been moved
        & #devices . #toDevice .~ toDeviceFun
        -- Overrides the default `loadModel` with one that handles caching for
        -- `TorchScript` models and getting configuration for `Bedrock` models
        & #models . #loadModel .~ loadModelFun
        & #models . #prompt .~ promptFun

    loadModelFun :: BridgeV RemoteM
    loadModelFun =
      VFun $ \case
        VCustom (VModelName (ModelName uuid)) ->
          either
            (throwM . RuntimeError . displayException)
            (pure . toValue)
            -- NOTE: `loadModel` internally handles the model version `contents`
            -- type. For `TorchScript` models, this includes potentially reading
            -- and writing the serialized blob to local storage if not cached;
            -- for `Bedrock` models, this just holds the Bedrock configuration in
            -- memory (which will be used by `prompt`)
            =<< liftImplEnvM (tryAny (loadModel uuid))
        _ -> throwM $ RuntimeError "loadModel: expected a modelName"

    toDeviceFun :: BridgeV RemoteM
    toDeviceFun =
      VFun $ \case
        VEnum _ e ->
          -- The device that the user wants to move the tensor to; currently
          -- can only be `#cpu` or `#cuda`
          getDevice e <&> \device ->
            -- Original tensor to be moved (as Inferno value)
            VFun $ \vtensor ->
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

    promptFun :: BridgeV RemoteM
    promptFun =
      VFun $ \case
        -- Note that the distinction between `Bedrock` and `TorchScript`
        -- models is not tracked by the type system, otherwise we'd
        -- have a lot of duplication between model things. Because
        -- of this, we need to handle mismatches in the runtime
        -- (e.g. `prompt`ing to a Torchscript model)
        VCustom (VModel (TorchScript _)) ->
          throwM . RuntimeError $
            unwords
              [ "Cannot `prompt` a TorchScript model; `prompt` is"
              , "only compatible with Bedrock models"
              ]
        VCustom (VModel (Bedrock config)) -> pure . VFun $ \case
          VText t -> undefined
          _ -> undefined
        _ -> throwM $ RuntimeError "prompt: expecting a model"

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
toDeviceIO device t0 =
  -- Catch any synchronous exceptions here and then inspect them. Note we need
  -- to `evaluate` the "pure" `Torch.toDevice` to surface impure exceptions
  -- (any `error` call)
  --
  -- It would be nice to use `tryAnyDeep` here but `Tensor` does not have an
  -- `NFData` instance
  tryAny (evaluate (Torch.toDevice device t0)) >>= \case
    -- The moved tensor. If the requested device is not available, e.g. moving
    -- to CUDA on a CPU-only device, this "pure" function will throw an `error`
    -- and we will hit the first `Left` case below
    Right t1 -> pure $ Right t1
    Left e
      -- If the `error` occurs, the original tensor is returned. This is because
      -- Hasktorch will `error` when the device is not correct (e.g. trying to
      -- move a tensor to `cuda:0` when only `cpu` is available). So in this case
      -- a warning is logged (in the script evaluator) and the original tensor is
      -- returned
      --
      -- Note that `Left` here indicates that the tensor was NOT moved, and a
      -- warning will be logged from script evaluator
      | isJust $ fromException @ErrorCall e -> pure $ Left t0
      | otherwise ->
          throwM . RuntimeError $
            unwords
              [ "Exception when moving tensor to device"
              , show device <> ":"
              , displayException e
              ]

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
   printWith : forall 'a. text -> 'a -> () := ###!printWithFun###;

   @doc Convert a value to text;
   show : forall 'a. 'a -> text := ###!showFun###;

  |]
  where
    showFun :: BridgeV m
    showFun = VFun $ pure . toValue . renderValue

renderValue :: (Pretty v) => Value v (ImplEnvM m v) -> Text
renderValue = renderStrict . layoutPretty defaultLayoutOptions . pretty

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

-- There should always be a bridge saved for the param
getBridgeInfo :: Id InferenceParam -> RemoteM BridgeInfo
getBridgeInfo ipid =
  firstOrThrow (NoBridgeSaved ipid) =<< queryStore q (Only ipid)
  where
    q :: Query
    q = [sql| SELECT * FROM bridges WHERE id = ? |]
