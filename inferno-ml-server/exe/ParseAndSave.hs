{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This executable evaluates an Inferno script and saves the hash and JSON
-- closure to the DB. This is necessary for testing because `inferno-ml-server`
-- expects the DB to contain script closures keyed by their hash. For real
-- deployments, this will be performed by `inferno-ml-orchestrator`, which
-- lives in a different repo

module ParseAndSave (main) where

import Control.Category ((>>>))
import Control.Exception (Exception (displayException))
import Control.Monad (void)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple
  ( Connection,
    Query,
    close,
    connectPostgreSQL,
    execute,
    withTransaction,
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Foreign.C (CTime)
import GHC.Generics (Generic)
import Inferno.Core
  ( Interpreter (Interpreter, parseAndInfer),
    mkInferno,
  )
import Inferno.ML.Module.Prelude (mlPrelude)
import Inferno.ML.Server.Module.Prelude (mkServerBridgePrelude, mkExtraModules)
import Inferno.ML.Types.Value (customTypes)
import Inferno.Module.Prelude (ModuleMap)
import Inferno.Types.Syntax (Expr, TCScheme)
import Inferno.Types.VersionControl
  ( Pinned,
    VCObjectHash,
    vcHash,
  )
import Inferno.VersionControl.Types
  ( VCObject (VCFunction),
    VCObjectPred (Init),
    VCObjectVisibility (VCObjectPublic),
  )
import System.Environment (getArgs)
import System.Exit (die)
import UnliftIO.Exception (bracket, throwString)
import "inferno-ml-server" Inferno.ML.Server.Types

main :: IO ()
main =
  getArgs >>= \case
    ustr : scriptp : pstr : conns : _ -> do
      pids <- either throwString pure $ eitherDecode (Lazy.Char8.pack pstr)
      ipid <-
        maybe (throwString "Invalid inference param Id") pure $
          UUID.fromString ustr
      parseAndSave (Id ipid) scriptp (Char8.pack conns) pids
    _ -> die "Usage ./parse <UUID> <SCRIPT-PATH> <PID-MAP-JSON> <DB-STR>"

parseAndSave ::
  Id InferenceParam ->
  FilePath ->
  ByteString ->
  InputsOutputs ->
  IO ()
parseAndSave ipid p conns ios = do
  t <- Text.IO.readFile p
  now <- fromIntegral @Int . round <$> getPOSIXTime
  ast <-
    either (throwString . displayException) pure . (`parse` t)
      =<< mkInferno prelude customTypes
  bracket (connectPostgreSQL conns) close (saveScriptAndParam ipid ast now ios)
  where
    prelude :: ModuleMap IO BridgeMlValue
    prelude = Map.union extraModules $ mkServerBridgePrelude funs mlPrelude

    extraModules :: ModuleMap IO BridgeMlValue
    extraModules = mkExtraModules notSupported notSupported

saveScriptAndParam ::
  Id InferenceParam ->
  (Expr (Pinned VCObjectHash) (), TCScheme) ->
  CTime ->
  InputsOutputs ->
  Connection ->
  IO ()
saveScriptAndParam ipid x now ios conn = insertScript *> insertParam
  where
    insertScript :: IO ()
    insertScript =
      void . withTransaction conn . execute conn q $
        InferenceScript hash vcmeta
      where
        q :: Query
        q =
          -- Bit of a hack. We only have one model version in the
          -- tests, so we can just hard-code the ID here
          [sql|
            WITH ins AS (
              INSERT INTO scripts (id, obj)
              VALUES (?, ?)
              RETURNING id
            )
            INSERT INTO mselections (script, model, ident)
              SELECT id, '00000006-0000-0000-0000-000000000000'::uuid, 'mnist'
            FROM ins
          |]

    insertParam :: IO ()
    insertParam = saveParam *> saveBridgeInfo
      where
        saveParam :: IO ()
        saveParam =
          void
            . withTransaction conn
            . execute conn q
            . InferenceParam
              (Just ipid)
              hash
              ios.inputs
              ios.outputs
              128
              Nothing
            $ entityIdFromInteger 0
          where
            q :: Query
            q =
              [sql|
                INSERT INTO params
                  ( id
                  , script
                  , inputs
                  , outputs
                  , resolution
                  , terminated
                  , gid
                  )
                VALUES (?, ?, ?, ?, ?, ?, ?)
              |]

        saveBridgeInfo :: IO ()
        saveBridgeInfo =
          void
            . withTransaction conn
            . execute conn q
            . flip (BridgeInfo ipid) 9999
            $ toIPv4 (127, 0, 0, 1)
          where
            q :: Query
            q = [sql| INSERT INTO bridges VALUES (?, ?, ?) |]

    vcfunc :: VCObject
    vcfunc = uncurry VCFunction x

    hash :: VCObjectHash
    hash = vcHash vcfunc

    vcmeta :: VCMeta VCObject
    vcmeta = VCMeta now smd gid "mnist" "A script" Init VCObjectPublic vcfunc

    smd :: ScriptMetadata
    smd = ScriptMetadata uid [inferenceScript] mempty
      where
        inferenceScript :: ScriptType
        inferenceScript =
          MLInferenceScript
            . InferenceOptions
            . Map.singleton "mnist"
            . Id
            $ UUID.fromWords 6 0 0 0

    uid :: EntityId UId
    uid = entityIdFromInteger 0

    gid :: EntityId GId
    gid = entityIdFromInteger 1

parse ::
  Interpreter m BridgeMlValue ->
  Text ->
  Either
    SomeInfernoError
    (Expr (Pinned VCObjectHash) (), TCScheme)
parse Interpreter{parseAndInfer} =
  parseAndInfer >>> \case
    Left e -> Left $ SomeInfernoError $ show e
    Right (x, t, _, _) -> Right (void x, t)

-- These are needed to parse the script, but do not need to do anything
funs :: BridgeFuns IO
funs = BridgeFuns notSupported notSupported notSupported notSupported

notSupported :: a
notSupported = error "Not supported"

data InputsOutputs = InputsOutputs
  { inputs :: Inputs PID
  , outputs :: Outputs PID
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)
