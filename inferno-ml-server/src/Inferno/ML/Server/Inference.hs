{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Inferno.ML.Server.Inference
  ( runInferenceParam,
    getAndCacheModel,
    linkVersionedModel,
  )
where

import Conduit (yieldMany)
import Control.Monad (void, when, (<=<))
import Control.Monad.Catch (throwM)
import Control.Monad.Extra (loopM, unlessM, whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.ListM (sortByM)
import Data.Bifoldable (bitraverse_)
import Data.Foldable (foldl')
import Data.Generics.Wrapped (wrappedTo)
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector as Vector
import Database.PostgreSQL.Simple
  ( Only (Only),
    Query,
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Foreign.C (CTime)
import Inferno.Core
  ( Interpreter (Interpreter, evalExpr, mkEnvFromClosure),
  )
import Inferno.ML.Server.Inference.Model
import Inferno.ML.Server.Types
import Inferno.ML.Server.Utils
import Inferno.ML.Types.Value (MlValue (VExtended))
import Inferno.Types.Syntax
  ( Expr (App, Var),
    ExtIdent (ExtIdent),
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
import System.FilePath (dropExtensions, (<.>))
import UnliftIO.Async (wait, withAsync)
import UnliftIO.Directory
  ( createFileLink,
    doesPathExist,
    getAccessTime,
    getCurrentDirectory,
    getFileSize,
    listDirectory,
    removeFile,
    removePathForcibly,
    withCurrentDirectory,
  )
import UnliftIO.Exception (bracket_, catchIO, displayException)
import UnliftIO.IO.File (writeBinaryFileDurableAtomic)
import UnliftIO.IORef (readIORef)
import UnliftIO.MVar (putMVar, takeMVar, withMVar)
import UnliftIO.Timeout (timeout)

-- Run an inference param, locking the `MVar` held in the `Env`. This is to avoid
-- running params in parallel (which may lead to problems with model caching,
-- etc...) and also to indicate whether any active process is running in the
-- `status` endpoint (using `tryTakeMVar`)
runInferenceParam ::
  Id InferenceParam ->
  Maybe Int64 ->
  RemoteM (WriteStream IO)
-- FIXME / TODO Deal with default resolution, probably shouldn't need to be
-- passed on all requests
runInferenceParam ipid (fromMaybe 128 -> res) =
  withTimeoutMillis $ \t -> do
    logTrace $ RunningInference ipid t
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
    runInference :: Int -> RemoteM (Maybe (WriteStream IO))
    runInference tmo = timeout tmo $ do
      view #interpreter >>= readIORef >>= \case
        Nothing -> throwM BridgeNotRegistered
        Just interpreter -> do
          param <- getParameter ipid
          obj <- getVcObject $ view #script param
          cache <- view $ #config . #cache
          t <- liftIO $ fromIntegral @Int . round <$> getPOSIXTime
          -- Change working directories to the model cache so that Hasktorch
          -- can find the models using relative paths (otherwise the AST would
          -- need to be updated to use an absolute path to a versioned model,
          -- e.g. `loadModel "~/inferno/.cache/..."`)
          withCurrentDirectory (view #path cache) $ do
            logTrace $ EvaluatingScript ipid
            linkVersionedModel =<< getAndCacheModel cache (view #model param)
            runEval interpreter param t obj
          where
            runEval ::
              Interpreter RemoteM BridgeMlValue ->
              InferenceParam ->
              CTime ->
              VCMeta VCObject ->
              RemoteM (WriteStream IO)
            runEval Interpreter {evalExpr, mkEnvFromClosure} param t vcm =
              vcm ^. #obj & \case
                VCFunction {} -> do
                  -- Need to set up envs for script eval
                  let mkIdent :: Int -> ExtIdent
                      mkIdent = ExtIdent . Right . ("input$" <>) . tshow

                      toSeries :: PID -> Value BridgeMlValue m
                      toSeries = VCustom . VExtended . VSeries

                      localEnv :: Map ExtIdent (Value BridgeMlValue m)
                      localEnv =
                        Map.fromList $
                          zip [0 ..] ps <&> \case
                            (i, Single pid) -> (mkIdent i, toSeries pid)
                            (i, Many pids) ->
                              ( mkIdent i,
                                pids ^.. each & over mapped toSeries & VArray
                              )
                        where
                          ps :: [SingleOrMany PID]
                          ps = param ^.. #inputs . each

                      closure :: Map VCObjectHash VCObject
                      closure =
                        param ^. #script
                          & ( `Map.singleton` view #obj vcm
                            )

                      expr :: Expr (Maybe VCObjectHash) ()
                      expr =
                        foldl'
                          App
                          ( Var () (preview #script param) LocalScope dummy
                          )
                          args
                        where
                          args :: [Expr (Maybe a) ()]
                          args =
                            [0 .. param ^. #inputs & Vector.length & (- 1)]
                              <&> Var () Nothing LocalScope
                                . Expl
                                . ExtIdent
                                . Right
                                . ("input$" <>)
                                . tshow

                          dummy :: ImplExpl
                          dummy = Expl . ExtIdent $ Right "dummy"

                  doEval expr =<< runImplEnvM mempty (mkEnvFromClosure localEnv closure)
                  where
                    doEval ::
                      Expr (Maybe VCObjectHash) () ->
                      BridgeTermEnv RemoteM ->
                      RemoteM (WriteStream IO)
                    doEval x env =
                      either
                        (throwInfernoError . Left . SomeInfernoError)
                        yieldPairs
                        =<< evalExpr env implEnv x

                    yieldPairs ::
                      Value BridgeMlValue (ImplEnvM RemoteM BridgeMlValue) ->
                      RemoteM (WriteStream IO)
                    yieldPairs (VArray vs) =
                      yieldMany . concat <$> mapM extractWrite vs
                    yieldPairs v =
                      throwM $ InvalidScript $ "Script output should be an array of `write` but was " <> renderPretty v

                    extractWrite ::
                      Value BridgeMlValue (ImplEnvM RemoteM BridgeMlValue) ->
                      RemoteM [WriteStreamItem]
                    extractWrite (VCustom (VExtended (VWrite (PID pid, vals)))) =
                      pure $ WritePid pid : map WriteValue vals
                    extractWrite v =
                      throwM $ InvalidScript $ "Script output should be an array of `write` but was " <> renderPretty v

                    implEnv :: Map ExtIdent (Value BridgeMlValue m)
                    implEnv =
                      Map.fromList
                        [ (ExtIdent $ Right "now", VEpochTime t),
                          ( ExtIdent $ Right "resolution",
                            VCustom . VExtended . VResolution $
                              toResolution res
                          )
                        ]
                _ ->
                  throwM
                    . InvalidScript
                    $ Text.unwords
                      [ "Script identified by VC hash",
                        param ^. #script & tshow,
                        "is not a function"
                      ]

    -- Convert the script timeout from seconds (for ease of configuration) to
    -- milliseconds, for use with `timeout`
    withTimeoutMillis :: (Int -> RemoteM b) -> RemoteM b
    withTimeoutMillis =
      (view (#config . #timeout) >>=)
        . (. (* 1000000) . fromIntegral)

getVcObject :: VCObjectHash -> RemoteM (VCMeta VCObject)
getVcObject vch =
  -- It's easier to `SELECT *` and then get the `obj` field, instead of
  -- `SELECT obj`, because the `FromRow` instance for `InferenceScript`
  -- deals with the JSON encoding of the `obj`
  fmap (view #obj) . firstOrThrow (NoSuchScript vch)
    =<< queryStore @_ @InferenceScript q (Only vch)
  where
    -- The script hash is used as the primary key in the table
    q :: Query
    q = [sql| SELECT * FROM scripts WHERE id = ? |]

-- Link the versioned versioned to the model, e.g. `<name>.<version>` to just
-- `<name>.ts.pt`, so it can be loaded by Hasktorch
linkVersionedModel :: FilePath -> RemoteM ()
linkVersionedModel withVersion = do
  whenM (doesPathExist withExt) $ removePathForcibly withExt
  createFileLink withVersion withExt
  where
    withExt :: FilePath
    withExt = dropExtensions withVersion <.> "ts" <.> "pt"

getParameter :: Id InferenceParam -> RemoteM InferenceParam
getParameter iid =
  firstOrThrow (NoSuchParameter (wrappedTo iid))
    =<< queryStore q (Only iid)
  where
    q :: Query
    q =
      [sql|
        SELECT * FROM params
        WHERE id = ? AND terminated IS NULL
        LIMIT 1
      |]

-- | First retrieves the specified model version from the database, then fetches
-- the associated parent model. The contents of the model version are retrieved
-- (the Postgres large object), then copied to the model cache if it has not
-- yet been cached. Older previously saved model versions(s) are evicted if the
-- cache 'maxSize' is exceeded by adding the model version contents
--
-- NOTE: This action assumes that the current working directory is the model
-- cache! It can be run using e.g. 'withCurrentDirectory'
getAndCacheModel :: ModelCache -> Id ModelVersion -> RemoteM FilePath
getAndCacheModel cache mid = do
  logTrace $ CopyingModel mid
  -- Both the individual version is required (in order to fetch the contents)
  -- as well as the parent model row (for the model name)
  mversion <- getModelVersion mid
  model <- getModel $ view #model mversion
  copyAndCache model mversion
  where
    copyAndCache :: Model -> ModelVersion -> RemoteM FilePath
    copyAndCache model mversion =
      versioned <$ do
        unlessM (doesPathExist versioned) $
          bitraverse_ checkCacheSize (writeBinaryFileDurableAtomic versioned)
            =<< getModelSizeAndContents (view #contents mversion)
      where
        -- Cache the model with its specific version, i.e.
        -- `<name>.ts.pt.<version>`, which will later be
        -- symlinked to `<name>.ts.pt`
        versioned :: FilePath
        versioned =
          model ^. #name . unpacked
            & (<.> view (#version . to showVersion . unpacked) mversion)

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
                  logTrace
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
        maxSize = cache ^. #maxSize & fromIntegral

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
