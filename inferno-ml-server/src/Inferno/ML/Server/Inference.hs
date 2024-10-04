{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
import Control.Monad (void, when, (<=<))
import Control.Monad.Catch (throwM)
import Control.Monad.Extra (loopM, unlessM, whenJust, whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.ListM (sortByM)
import Data.Bifoldable (bitraverse_)
import Data.Bifunctor (bimap)
import Data.Conduit.List (chunksOf, sourceList)
import Data.Foldable (foldl', traverse_)
import Data.Generics.Wrapped (wrappedFrom, wrappedTo)
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
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
    SqlError,
  )
import Database.PostgreSQL.Simple.Newtypes (getAeson)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Foreign.C (CTime)
import GHC.Generics (Generic)
import Inferno.Core
  ( Interpreter (Interpreter, evalExpr, mkEnvFromClosure),
  )
import Inferno.ML.Server.Bridge (initializeInferno)
import Inferno.ML.Server.Inference.Model
import Inferno.ML.Server.Types
import Inferno.ML.Server.Utils
import Inferno.ML.Types.Value (MlValue (VExtended, VModelName))
import Inferno.Types.Syntax
  ( Expr (App, Var),
    ExtIdent (ExtIdent),
    Ident,
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
    catch,
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
    =<< getParameterWithModels ipid
  where
    mkScriptEnv :: InferenceParamWithModels -> RemoteM ScriptEnv
    mkScriptEnv pwm =
      ScriptEnv pwm.param pwm.models pwm.param.inputs
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
    =<< getParam
  where
    -- Note that, unlike `runInferenceParam`, several of the items required
    -- for script eval come from the `EvaluationEnv`
    mkScriptEnv :: InferenceParam -> RemoteM ScriptEnv
    mkScriptEnv param =
      ScriptEnv param eenv.models eenv.inputs
        <$> getVcObject eenv.script
        ?? eenv.script
        ?? mres

    getParam :: RemoteM InferenceParam
    getParam =
      firstOrThrow (NoSuchParameter ipid)
        =<< queryStore q (Only ipid)
      where
        q :: Query
        q = [sql| SELECT * FROM params WHERE id = ? |]

runInferenceParamWithEnv ::
  Id InferenceParam ->
  UUID ->
  ScriptEnv ->
  RemoteM (WriteStream IO)
runInferenceParamWithEnv ipid uuid senv =
  withTimeoutMillis $ \t -> do
    logInfo $ RunningInference ipid t
    maybe (throwM (ScriptTimeout t)) pure
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
      cache <- view $ #config . #cache
      t <- liftIO $ fromIntegral @Int . round <$> getPOSIXTime
      -- Change working directories to the model cache so that Hasktorch
      -- can find the models using relative paths (otherwise the AST would
      -- need to be updated to use an absolute path to a versioned model,
      -- e.g. `loadModel "~/inferno/.cache/..."`)
      withCurrentDirectory cache.path $ do
        logInfo $ EvaluatingParam ipid
        getAndCacheModels cache $ senv.models
        runEval interpreter t
      where
        runEval ::
          Interpreter RemoteM BridgeMlValue ->
          CTime ->
          RemoteM (WriteStream IO)
        runEval Interpreter {evalExpr, mkEnvFromClosure} t =
          senv ^. #obj . #obj & \case
            VCFunction {} -> do
              let -- Note that this both includes inputs (i.e. readable)
                  -- and outputs (i.e. writable, or readable/writable).
                  -- These need to be provided to the script in order
                  -- for the symbolic identifer (e.g. `output0`) to
                  -- resolve. We can discard the input type here,
                  -- however. The distinction is only relevant for the
                  -- runtime that runs as a script evaluation engine
                  -- and commits the output write object
                  pids :: [SingleOrMany PID]
                  pids =
                    senv ^.. #inputs . to Map.toAscList . each . _2 . _1

                  -- List of model versions, which are used to evaluate
                  -- `loadModel` primitive (eventually calling Hasktorch to
                  -- load the script module)
                  models :: [Id ModelVersion]
                  models = senv ^.. #models . to Map.toAscList . each . _2

                  mkIdentWith :: Text -> Int -> ExtIdent
                  mkIdentWith x = ExtIdent . Right . (x <>) . tshow

                  toSeries :: PID -> Value BridgeMlValue m
                  toSeries = VCustom . VExtended . VSeries

                  toModelPath :: Id ModelVersion -> Value BridgeMlValue m
                  toModelPath = VCustom . VModelName . wrappedFrom . mkModelPath

                  argsFrom ::
                    [a] ->
                    ((Int, a) -> (ExtIdent, Value BridgeMlValue m)) ->
                    [(ExtIdent, Value BridgeMlValue m)]
                  argsFrom xs f = f <$> zip [0 ..] xs

                  localEnv :: Map ExtIdent (Value BridgeMlValue m)
                  localEnv = Map.fromList $ inputArgs <> modelArgs
                    where
                      inputArgs :: [(ExtIdent, Value BridgeMlValue m)]
                      inputArgs =
                        argsFrom pids $ \case
                          (i, Single pid) ->
                            ( mkIdentWith "input$" i,
                              toSeries pid
                            )
                          (i, Many pids') ->
                            ( mkIdentWith "input$" i,
                              pids' ^.. each & over mapped toSeries & VArray
                            )

                      modelArgs :: [(ExtIdent, Value BridgeMlValue m)]
                      modelArgs =
                        argsFrom models $
                          bimap (mkIdentWith "model$") toModelPath

                  closure :: Map VCObjectHash VCObject
                  closure = senv ^. #obj . #obj & Map.singleton senv.script

                  expr :: Expr (Maybe VCObjectHash) ()
                  expr =
                    flip (foldl' App) args $
                      Var () (Just senv.script) LocalScope dummy
                    where
                      -- See note above about inputs/outputs
                      args :: [Expr (Maybe a) ()]
                      args = exprsFrom "input$" pids <> exprsFrom "model$" models
                        where
                          exprsFrom :: Text -> [a] -> [Expr (Maybe b) ()]
                          exprsFrom ident xs =
                            [0 .. length xs - 1]
                              <&> Var () Nothing LocalScope
                                . Expl
                                . ExtIdent
                                . Right
                                . (ident <>)
                                . tshow

                      dummy :: ImplExpl
                      dummy = Expl . ExtIdent $ Right "dummy"

              either (throwInfernoError . Left . SomeInfernoError) yieldPairs
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
                      v -> throwM . InvalidOutput $ renderPretty v
                  v -> throwM . InvalidOutput $ renderPretty v
                  where
                    mkChunks ::
                      ConduitT
                        (PID, [(EpochTime, IValue)])
                        (Int, [(EpochTime, IValue)])
                        IO
                        ()
                    mkChunks = awaitForever $ \(p, ws) ->
                      sourceList ws .| chunksOf 500 .| mapC (wrappedTo p,)

                -- If the optional resolution override has been provided,
                -- use that. Otherwise, use the resolution stored in the
                -- parameter
                resolution :: InverseResolution
                resolution = senv ^. #mres . non res & toResolution
                  where
                    res :: Int64
                    res = senv ^. #param . #resolution & fromIntegral

                implEnv :: Map ExtIdent (Value BridgeMlValue m)
                implEnv =
                  Map.fromList
                    [ (ExtIdent $ Right "now", VEpochTime t),
                      ( ExtIdent $ Right "resolution",
                        VCustom . VExtended $ VResolution resolution
                      )
                    ]
            _ ->
              throwM
                . InvalidScript
                $ Text.unwords
                  [ "Script identified by VC hash",
                    tshow senv.script,
                    "is not a function"
                  ]

    -- Convert the script timeout from seconds (for ease of configuration) to
    -- milliseconds, for use with `timeout`; then execute the action
    withTimeoutMillis :: (Int -> RemoteM b) -> RemoteM b
    withTimeoutMillis =
      (view (#config . #timeout) >>=)
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

      ws <$ r (saveEvaluationInfo (end, start) (bytes1, bytes0) (cpu1, cpu0))
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
          insert `catch` logAndIgnore
          where
            insert :: RemoteM ()
            insert =
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

            -- We don't want a DB error to completely break inference
            -- evaluation. Inability to store the eval info is more of
            -- an inconvenience than a fatal error
            logAndIgnore :: SqlError -> RemoteM ()
            logAndIgnore =
              logWarn
                . OtherWarn
                . ("Failed to save eval info: " <>)
                . Text.pack
                . displayException

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

getParameterWithModels :: Id InferenceParam -> RemoteM InferenceParamWithModels
getParameterWithModels ipid =
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
    q :: Query
    q =
      [sql|
        SELECT P.*, jsonb_object_agg(MS.ident, MS.model) mversions
        FROM params P
          INNER JOIN scripts S ON P.script = S.id
          INNER JOIN mselections MS ON MS.script = S.id
          INNER JOIN mversions MV ON MV.id = MS.model
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
getAndCacheModels ::
  ModelCache -> Map Ident (Id ModelVersion) -> RemoteM ()
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
          bitraverse_ checkCacheSize (writeBinaryFileDurableAtomic path)
            =<< getModelVersionSizeAndContents mversion.contents
      where
        mkPath :: RemoteM FilePath
        mkPath =
          maybe (throwM (OtherRemoteError "todo")) (pure . mkModelPath) $
            mversion.id

    -- Checks that the configured cache size will not be exceeded by
    -- caching the new model. If it will, least-recently-used models
    -- are deleted until there is enough free space
    checkCacheSize :: Integer -> RemoteM ()
    checkCacheSize modelSize = do
      when (modelSize >= maxSize) $ throwM CacheSizeExceeded
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
modelsByAccessTime :: forall m. MonadIO m => FilePath -> m [FilePath]
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
  { param :: InferenceParam,
    models :: Map Ident (Id ModelVersion),
    inputs :: Map Ident (SingleOrMany PID, ScriptInputType),
    obj :: VCMeta VCObject,
    script :: VCObjectHash,
    mres :: Maybe Int64
  }
  deriving stock (Generic)
