{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Inferno.ML.Server.Inference
  ( runInferenceParam,
    testInferenceParam,
    getAndCacheModels,
  )
where

import Conduit (ConduitT, awaitForever, mapC, yieldMany, (.|))
import Control.Applicative (asum)
import Control.Monad (unless, void, when, (<=<))
import Control.Monad.Extra (loopM, unlessM, whenJust, whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.ListM (sortByM)
import Data.Conduit.List (chunksOf, sourceList)
import Data.Foldable (foldl', toList, traverse_)
import Data.Generics.Wrapped (wrappedFrom, wrappedTo)
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Traversable (for)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import Data.Word (Word64)
import Database.PostgreSQL.Simple
  ( Only (Only, fromOnly),
    Query,
  )
import Database.PostgreSQL.Simple.Newtypes (getAeson)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (PGArray (PGArray))
import Foreign.C (CTime)
import GHC.Generics (Generic)
import Inferno.Core
  ( Interpreter (Interpreter, evalExpr, mkEnvFromClosure),
  )
import Inferno.ML.Server.Bridge (initializeInferno)
import Inferno.ML.Server.Inference.Model
import Inferno.ML.Server.Types
import Inferno.ML.Server.Utils
import Inferno.ML.Types.Value
  ( pattern VExtended,
    pattern VModelName,
  )
import Inferno.Types.Syntax
  ( Expr (App, Lam, Var),
    ExtIdent (ExtIdent),
    Ident (Ident),
    ImplExpl (Expl),
    Scoped (LocalScope),
  )
import Inferno.Types.Value
  ( ImplEnvM,
    Value (VArray, VCustom, VEpochTime),
    runImplEnvM,
  )
import Inferno.Types.VersionControl (VCObjectHash)
import Inferno.Utils.Prettyprinter (renderPretty)
import Inferno.VersionControl.Types
  ( VCObject (VCFunction),
  )
import qualified Inferno.VersionControl.Types
import Lens.Micro.Platform
import System.CPUTime (getCPUTime)
import System.FilePath ((<.>))
import System.Mem (getAllocationCounter, setAllocationCounter)
import System.Posix.Types (EpochTime)
import UnliftIO (withRunInIO)
import UnliftIO.Async (wait, withAsync)
import UnliftIO.Directory
  ( doesPathExist,
    getAccessTime,
    getCurrentDirectory,
    getFileSize,
    listDirectory,
    removeFile,
    withCurrentDirectory,
  )
import UnliftIO.Exception
  ( bracket_,
    catchIO,
    displayException,
  )
import UnliftIO.IO.File (writeBinaryFileDurableAtomic)
import UnliftIO.IORef (atomicWriteIORef, readIORef)
import UnliftIO.MVar (putMVar, takeMVar, withMVar)
import UnliftIO.Timeout (timeout)

-- | Run an inference param, locking the @MVar@ held in the 'Env'. This is to
-- avoid running params in parallel (which may lead to problems with model
-- caching, etc...) and also to indicate whether any active process is running
-- in the @/status@ endpoint (using @tryTakeMVar@)
runInferenceParam ::
  Id InferenceParam ->
  -- | Optional resolution. If not provided, @InferenceParam.resolution@ will
  -- be used. This is provided in order to allow users to override the stored
  -- resolution without needing to alter the DB
  Maybe Int64 ->
  UUID ->
  RemoteM (WriteStream IO)
runInferenceParam ipid mres uuid =
  runInferenceParamWithEnv ipid uuid
    =<< mkScriptEnv
    =<< getInferenceParamWithModels ipid
  where
    mkScriptEnv :: InferenceParamWithModels -> RemoteM ScriptEnv
    mkScriptEnv pwm =
      ScriptEnv pwm.param pwm.models pwm.param.inputs pwm.param.outputs
        <$> getVcObject pwm.param.script
          ?? pwm.param.script
          ?? mres

-- | Test an inference param. This requires a script object to be saved to
-- the DB, but it is does not need to be linked to the parameter itself. It
-- also allows for overriding models and inputs, which normally need to be
-- fixed to the script or param, respectively
testInferenceParam ::
  Id InferenceParam ->
  Maybe Int64 ->
  UUID ->
  EvaluationEnv ->
  RemoteM (WriteStream IO)
testInferenceParam ipid mres uuid eenv =
  runInferenceParamWithEnv ipid uuid
    =<< mkScriptEnv
    -- Just need to get the param, we already have the model information
    -- from the overrides
    =<< getInferenceParamWithModels ipid
  where
    -- Note that, unlike `runInferenceParam`, several of the items required
    -- for script eval MAY come from the `EvaluationEnv` if they have been
    -- overridden. See the bindings for `inputs`, `outputs`, and `models` below
    -- for an explanation
    mkScriptEnv :: InferenceParamWithModels -> RemoteM ScriptEnv
    mkScriptEnv pwm =
      ScriptEnv pwm.param models inputs outputs
        <$> getVcObject eenv.script
          ?? eenv.script
          ?? mres
      where
        -- If the `inputs` have not been specified in the evaluation env, i.e.
        -- the inputs are not being overridden, use the ones that are linked
        -- directly to the param
        inputs :: Inputs PID
        inputs
          | null eenv.inputs = pwm.param.inputs
          | otherwise = eenv.inputs

        -- Likewise, if the `outputs` have not been overridden, use the ones
        -- that are linked directly to the param
        outputs :: Outputs PID
        outputs
          | null eenv.outputs = pwm.param.outputs
          | otherwise = eenv.outputs

        -- Likewise, if the `models` have not been overridden, use the ones
        -- that are linked directly to the param via its inference script
        models :: Models (Id ModelVersion)
        models
          | null eenv.models = pwm.models
          | otherwise = eenv.models

runInferenceParamWithEnv ::
  Id InferenceParam ->
  UUID ->
  ScriptEnv ->
  RemoteM (WriteStream IO)
runInferenceParamWithEnv ipid uuid senv =
  -- This enforces that the script will finish execution within its given
  -- time limit (`withTimeoutMillis`) and that it will not consume up to or
  -- beyond the `MemoryMax` defined in its systemd service configuration
  withMemoryMonitor . withTimeoutMillis $ \t -> do
    logInfo $ RunningInference ipid t
    -- Clear the "console" before running the script, so any calls to
    -- `Print.print` will write to a fresh console
    (`atomicWriteIORef` mempty) =<< view #console
    maybe (throwRemoteError (ScriptTimeout ipid t)) pure
      =<< (`withMVar` const (run t))
      =<< view #lock
  where
    -- Runs the inference parameter in a separate `Async` thread. The `Async`
    -- is stored in the server environment so it can be canceled at any point
    -- before the script finishes evaluating
    run :: Int -> RemoteM (Maybe (WriteStream IO))
    run tmo = withAsync (runInference tmo) $ \a ->
      bracket_
        ((`putMVar` (ipid, a)) =<< view #job)
        (void . takeMVar =<< view #job)
        $ wait a

    -- Actually runs the inference evaluation, within the configured timeout
    --
    -- NOTE: Do not fork anything else inside here; this is already running
    -- in an `Async` and we want to be able to get execution statistics from
    -- the runtime. Specifically, we are using `getAllocationCounter`, but
    -- this only captures the allocations _in this thread only_
    runInference :: Int -> RemoteM (Maybe (WriteStream IO))
    runInference tmo = timeout tmo . withEvaluationInfo $ do
      -- If this is the first request after the server starts, the interpreter
      -- will not have been initialized yet. After that, it will be reused
      -- until the server is started again
      interpreter <- getOrMkInferno ipid
      cache <- view $ #config . #global . #cache
      t <- liftIO $ fromIntegral @Int . round <$> getPOSIXTime
      -- Change working directories to the model cache so that Hasktorch
      -- can find the models using relative paths (otherwise the AST would
      -- need to be updated to use an absolute path to a versioned model,
      -- e.g. `loadModel "~/inferno/.cache/..."`)
      withCurrentDirectory cache.path $ do
        logInfo $ EvaluatingParam ipid
        getAndCacheModels cache senv.models
        runEval interpreter t
      where
        runEval ::
          Interpreter RemoteM BridgeMlValue ->
          CTime ->
          RemoteM (WriteStream IO)
        runEval Interpreter{evalExpr, mkEnvFromClosure} t =
          case senv.obj.obj of
            -- We need the `vexpr` here, which is a `Lam`, to get ALL of the
            -- script arguments
            VCFunction vexpr _ -> do
              let
                -- Extract script parameter order from the lambda expression.
                -- This gives us the ORDER in which params should be applied.
                -- We filter out implicit type params (i.e. `Left _`) as they
                -- don't need runtime values
                scriptParamOrder :: [Ident]
                scriptParamOrder = mapMaybe toIdent $ extractLamParams vexpr
                  where
                    extractLamParams :: Expr h p -> [Maybe ExtIdent]
                    extractLamParams = go mempty
                      where
                        go :: [Maybe ExtIdent] -> Expr h p -> [Maybe ExtIdent]
                        go acc = \case
                          Lam _ xs _ e -> flip go e $ acc <> fmap snd (NonEmpty.toList xs)
                          _ -> acc

                    toIdent :: Maybe ExtIdent -> Maybe Ident
                    toIdent = \case
                      Just (ExtIdent (Right name)) -> Just (Ident name)
                      _ -> Nothing

                -- All params from the env - this is the source of truth for
                -- WHAT args to provide
                envPids :: Map Ident (SingleOrMany PID)
                envPids = senv.inputs <> senv.outputs

                envModels :: Map Ident (Id ModelVersion)
                envModels = senv.models

                -- Build args list by ordering env params according to script order.
                -- Params in the env but not in the script order go at the end
                -- (this handles any edge cases where the script has extra params)
                scriptArgs :: [ScriptArg]
                scriptArgs = orderedArgs <> extraArgs
                  where
                    -- Args in script param order
                    orderedArgs :: [ScriptArg]
                    orderedArgs = mapMaybe lookupInEnv scriptParamOrder

                    -- Any env params not in the script order
                    extraArgs :: [ScriptArg]
                    extraArgs =
                      let scriptSet :: Set Ident
                          scriptSet = Set.fromList scriptParamOrder

                          extraPids :: [ScriptArg]
                          extraPids =
                            Map.toList envPids
                              & filter ((`Set.notMember` scriptSet) . fst)
                              & fmap (ArgIoParam . snd)

                          extraModels :: [ScriptArg]
                          extraModels =
                            Map.toList envModels
                              & filter ((`Set.notMember` scriptSet) . fst)
                              & fmap (ArgModel . snd)
                       in extraPids <> extraModels

                    lookupInEnv :: Ident -> Maybe ScriptArg
                    lookupInEnv i =
                      asum
                        [ ArgIoParam <$> Map.lookup i envPids
                        , ArgModel <$> Map.lookup i envModels
                        ]

                mkIdent :: Int -> ExtIdent
                mkIdent = ExtIdent . Right . ("arg$" <>) . tshow

                toSeries :: PID -> Value BridgeMlValue m
                toSeries = VCustom . VExtended . VSeries

                toModelPath :: Id ModelVersion -> Value BridgeMlValue m
                toModelPath = VCustom . VModelName . wrappedFrom . mkModelPath

                -- Build local environment from unified args list
                localEnv :: Map ExtIdent (Value BridgeMlValue m)
                localEnv = Map.fromList $ zipWith mkEnvEntry [0 ..] scriptArgs
                  where
                    mkEnvEntry :: Int -> ScriptArg -> (ExtIdent, Value BridgeMlValue m)
                    mkEnvEntry i = \case
                      ArgIoParam (Single pid) -> (mkIdent i, toSeries pid)
                      ArgIoParam (Many pids) ->
                        ( mkIdent i
                        , pids ^.. each & over mapped toSeries & VArray
                        )
                      ArgModel mid -> (mkIdent i, toModelPath mid)

                closure :: Map VCObjectHash VCObject
                closure = Map.singleton senv.script senv.obj.obj

                -- Build application expression with args in exact script order
                expr :: Expr (Maybe VCObjectHash) ()
                expr =
                  flip (foldl' App) args $
                    Var () (Just senv.script) LocalScope dummy
                  where
                    args :: [Expr (Maybe a) ()]
                    args =
                      [0 .. length scriptArgs - 1]
                        <&> Var () Nothing LocalScope
                        . Expl
                        . mkIdent

                    dummy :: ImplExpl
                    dummy = Expl . ExtIdent $ Right "dummy"

              either (throwInfernoError ipid senv.script) yieldPairs
                =<< flip (`evalExpr` implEnv) expr
                =<< runImplEnvM mempty (mkEnvFromClosure localEnv closure)
              where
                yieldPairs ::
                  Value BridgeMlValue (ImplEnvM RemoteM BridgeMlValue) ->
                  RemoteM (WriteStream IO)
                yieldPairs = \case
                  VArray vs ->
                    fmap ((.| mkChunks) . yieldMany) . for vs $ \case
                      VCustom (VExtended (VWrite vw)) -> pure vw
                      v -> throwRemoteError . InvalidOutput ipid $ renderPretty v
                  v -> throwRemoteError . InvalidOutput ipid $ renderPretty v
                  where
                    mkChunks ::
                      ConduitT
                        (PID, [(IValue, EpochTime)])
                        (Int, [(IValue, EpochTime)])
                        IO
                        ()
                    mkChunks = awaitForever $ \(p, ws) ->
                      sourceList ws .| chunksOf 500 .| mapC (wrappedTo p,)

                -- If the optional resolution override has been provided,
                -- use that. Otherwise, use the resolution stored in the
                -- parameter
                resolution :: InverseResolution
                resolution =
                  senv
                    ^. #mres
                    . non (fromIntegral senv.param.resolution)
                    & toResolution

                implEnv :: Map ExtIdent (Value BridgeMlValue m)
                implEnv =
                  Map.fromList
                    [ (ExtIdent $ Right "now", VEpochTime t)
                    ,
                      ( ExtIdent $ Right "resolution"
                      , VCustom . VExtended $ VResolution resolution
                      )
                    ]
            _ ->
              throwRemoteError
                . InvalidScript ipid
                $ Text.unwords
                  [ "Script identified by VC hash"
                  , tshow senv.script
                  , "is not a function"
                  ]

    -- Convert the script timeout from seconds (for ease of configuration) to
    -- milliseconds, for use with `timeout`; then execute the action
    withTimeoutMillis :: (Int -> RemoteM b) -> RemoteM b
    withTimeoutMillis =
      (view (#config . #global . #timeout) >>=)
        . (. (* 1_000_000) . fromIntegral)

    withEvaluationInfo :: RemoteM a -> RemoteM a
    withEvaluationInfo f = withRunInIO $ \r -> do
      -- So allocation counter doesn't go below the lower limit, which is
      -- unlikely but should be accounted for at any rate
      setAllocationCounter maxBound
      start <- getCurrentTime
      bytes0 <- getAllocationCounter
      cpu0 <- getCPUTime
      ws <- r f
      end <- getCurrentTime
      bytes1 <- getAllocationCounter
      cpu1 <- getCPUTime
      r $ saveEvaluationInfo (end, start) (bytes1, bytes0) (cpu1, cpu0)
      -- The console needs to be written here, after evaluation info is saved,
      -- since the ID of the `consoles` column is an fkey pointing to the `evalinfo`
      -- pkey
      r writeConsole
      pure ws
      where
        saveEvaluationInfo ::
          -- End and start times
          (UTCTime, UTCTime) ->
          -- Ending and beginning byte allocation
          (Int64, Int64) ->
          -- Ending and beginning CPU time
          (Integer, Integer) ->
          RemoteM ()
        saveEvaluationInfo (end, start) (bytes1, bytes0) (cpu1, cpu0) =
          executeStore q $
            EvaluationInfo uuid ipid start end allocated cpuMillis
          where
            q :: Query
            q = [sql| INSERT INTO evalinfo VALUES (?, ?, ?, ?, ?, ?) |]

            -- Note that the allocation counter counts *down*, so we need to
            -- subtract the second value from the first value
            allocated :: Word64
            allocated =
              fromIntegral
                -- In the unlikely event that more memory was freed in
                -- this thread between the beginning of evaluation and
                -- the end, so we don't end up with `maxBound @Word64`
                . max 0
                $ bytes0 - bytes1

            -- Convert the picoseconds of CPU time to milliseconds
            cpuMillis :: Word64
            cpuMillis = fromIntegral $ (cpu1 - cpu0) `div` 1_000_000_000

        -- Save anything printed to the "console" via `Print.print` to
        -- the DB so it can be retrieved later
        writeConsole :: RemoteM ()
        writeConsole =
          view #console >>= readIORef >>= \console ->
            -- There's no point in adding a row if no `print`s were evaluated,
            -- we can always return an empty array as a default when querying
            -- the console
            unless (null console) . executeStore q . (uuid,) . PGArray $
              toList console
          where
            q :: Query
            q = [sql| INSERT INTO consoles (id, prints) VALUES (?, ?) |]

getOrMkInferno ::
  Id InferenceParam -> RemoteM (Interpreter RemoteM BridgeMlValue)
getOrMkInferno ipid =
  maybe (saveInterpreter =<< initializeInferno ipid) pure
    =<< readIORef
    =<< view #interpreter
  where
    saveInterpreter ::
      Interpreter RemoteM BridgeMlValue ->
      RemoteM (Interpreter RemoteM BridgeMlValue)
    saveInterpreter i =
      i <$ do
        (`atomicWriteIORef` Just i) =<< view #interpreter

getVcObject :: VCObjectHash -> RemoteM (VCMeta VCObject)
getVcObject vch =
  -- It's easier to `SELECT *` and then get the `obj` field, instead of
  -- `SELECT obj`, because the `FromRow` instance for `InferenceScript`
  -- deals with the JSON encoding of the `obj`
  fmap (.obj) . firstOrThrow (NoSuchScript vch)
    =<< queryStore @_ @InferenceScript q (Only vch)
  where
    -- The script hash is used as the primary key in the table
    q :: Query
    q = [sql| SELECT * FROM scripts WHERE id = ? |]

getInferenceParamWithModels :: Id InferenceParam -> RemoteM InferenceParamWithModels
getInferenceParamWithModels ipid =
  fmap
    ( uncurry InferenceParamWithModels
        . fmap (getAeson . fromOnly)
        . joinToTuple
    )
    . firstOrThrow (NoSuchParameter ipid)
    =<< queryStore q (Only ipid)
  where
    -- This query is somewhat complex in order to get all relevent information
    -- for creating the script evaluator's Inferno environment.
    --
    -- For each row in the `mselections` table linked to the param's script,
    -- it selects the model version. To create the `Map Ident (Id ModelVersion)`
    -- that is used in `ParamWithModels`, it generates a JSONB object. For
    -- example, given the following rows from `mselections`:
    --
    --     ```
    --     | script          | id              | ident      |
    --     -                 -                 -            -
    --     | \x123abc...     | 00000-0000...   | 'model0'   |
    --     | \x123abc...     | 00001-0000...   | 'model1'   |
    --     ```
    --
    -- the query will create the following JSONB object:
    --
    --     ```
    --     {
    --        "model0": "00000-0000...",
    --        "model1": "00001-0000..."
    --     }
    --     ```
    --
    -- If a script has no model attached, a default empty JSON object is
    -- returned
    q :: Query
    q =
      [sql|
        SELECT
          P.*
        , coalesce
          ( jsonb_object_agg(MS.ident, MS.model)
              FILTER (WHERE MS.ident IS NOT NULL)
          , '{}'::jsonb
          ) mversions
        FROM params P
          LEFT OUTER JOIN scripts S ON P.script = S.id
          LEFT OUTER JOIN mselections MS ON MS.script = S.id
          LEFT OUTER JOIN mversions MV ON MV.id = MS.model
        WHERE P.id = ?
          AND P.terminated IS NULL
        GROUP BY
          P.id
      |]

-- | For all of the model version IDs declared in the param, fetch the model
-- version and the parent model, and then cache them
--
-- The contents of the model version are retrieved (the Postgres large object),
-- then copied to the model cache if it has not yet been cached. Previously
-- saved model versions(s) are evicted if the cache 'maxSize' is exceeded by
-- adding the model version contents; this is based on access time
--
-- NOTE: This action assumes that the current working directory is the model
-- cache! It can be run using e.g. 'withCurrentDirectory'
--
-- NOTE/TODO In the future, once the @Bedrock@ model system is more complete,
-- this will ONLY be invoked if the model version holds a @Torchscript@ model.
-- At the moment, that is a more intrusive change that is not yet necessary
getAndCacheModels :: ModelCache -> Models (Id ModelVersion) -> RemoteM ()
getAndCacheModels cache =
  traverse_ (uncurry copyAndCache)
    <=< getModelsAndVersions
      . Vector.fromList
      . toListOf each
  where
    copyAndCache :: Model -> ModelVersion -> RemoteM ()
    copyAndCache _ mversion =
      mkPath >>= \path ->
        unlessM (doesPathExist path) $ do
          whenJust mversion.id $ logInfo . CopyingModel
          checkCacheSize $ fromIntegral mversion.size
          writeBinaryFileDurableAtomic path
            =<< getTorchScriptModelContents mversion
      where
        mkPath :: RemoteM FilePath
        mkPath =
          maybe
            (throwRemoteError (OtherRemoteError "Missing model version ID"))
            (pure . mkModelPath)
            mversion.id

        -- Checks that the configured cache size will not be exceeded by
        -- caching the new model. If it will, least-recently-used models
        -- are deleted until there is enough free space
        checkCacheSize :: Integer -> RemoteM ()
        checkCacheSize modelSize = do
          when (modelSize >= maxSize) $ throwRemoteError CacheSizeExceeded
          whenM cacheSizeExceeded evictOldModels
          where
            evictOldModels :: RemoteM ()
            evictOldModels =
              loopM doEvict
                =<< modelsByAccessTime
                -- Note that the current directory is the cache, because this
                -- action is only run above using `withCurrentDirectory` pointing
                -- to the cache
                =<< getCurrentDirectory

            doEvict :: [FilePath] -> RemoteM (Either [FilePath] ())
            doEvict = \case
              [] -> pure $ Right ()
              -- The list of paths is sorted in ascending order based on access
              -- time, so whatever path is at the head of the list is the current
              -- least-recently-used path
              m : ms ->
                cacheSizeExceeded >>= \case
                  False -> pure $ Right ()
                  True -> Left ms <$ tryRemoveFile
                where
                  tryRemoveFile :: RemoteM ()
                  tryRemoveFile =
                    catchIO (removeFile m) $
                      logWarn
                        . OtherWarn
                        . Text.pack
                        . displayException

            -- Adds the new model byte size to the existing model cache
            -- directory size. This needs to be re-computed on each loop
            -- iteration because models may have been deleted in the loop
            cacheSizeExceeded :: RemoteM Bool
            cacheSizeExceeded = (>= maxSize) <$> newCacheSize
              where
                newCacheSize :: RemoteM Integer
                newCacheSize =
                  fmap ((+ modelSize) . sum) . traverse getFileSize
                    =<< listDirectory
                    =<< getCurrentDirectory

            maxSize :: Integer
            maxSize = fromIntegral cache.maxSize

-- Get a list of models by their access time, so that models that have not been
-- used recently can be deleted. This will put the least-recently-used paths
-- at the head of the list
modelsByAccessTime :: forall m. (MonadIO m) => FilePath -> m [FilePath]
modelsByAccessTime = sortByM compareAccessTime <=< listDirectory
  where
    compareAccessTime :: FilePath -> FilePath -> m Ordering
    compareAccessTime f1 f2 =
      getAccessTime f1 >>= \t1 ->
        compare t1 <$> getAccessTime f2

-- There should only be one way to generate a filepath from a model version, so
-- that the path pointing to the contents is always unambiguous. This uses its
-- UUID to do so
mkModelPath :: Id ModelVersion -> FilePath
mkModelPath = (<.> "ts" <.> "pt") . UUID.toString . wrappedTo

-- Everything needed to evaluate an ML script. For the normal endpoint, all of
-- these will be derived directly from the param. For the interactive test
-- endpoint, these will be overridden
data ScriptEnv = ScriptEnv
  { param :: InferenceParam
  , models :: Models (Id ModelVersion)
  , inputs :: Inputs PID
  , outputs :: Outputs PID
  , obj :: VCMeta VCObject
  , script :: VCObjectHash
  , mres :: Maybe Int64
  }
  deriving stock (Generic)

-- A script argument is either a PID (for inputs/outputs) or a model version.
-- This type is used to build a unified argument list that preserves the exact
-- parameter order from the script's lambda expression
data ScriptArg
  = ArgIoParam (SingleOrMany PID)
  | ArgModel (Id ModelVersion)
  deriving stock (Show)
