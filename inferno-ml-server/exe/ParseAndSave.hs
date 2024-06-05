{-# LANGUAGE DataKinds #-}
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
import Data.Aeson (eitherDecode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector as Vector
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
import Inferno.Core
  ( Interpreter (Interpreter, parseAndInfer),
    mkInferno,
  )
import Inferno.ML.Server.Module.Prelude (mkBridgePrelude)
import Inferno.ML.Server.Types
import Inferno.ML.Types.Value (customTypes)
import Inferno.Types.Syntax (Expr, Ident, TCScheme)
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

main :: IO ()
main =
  getArgs >>= \case
    scriptp : pstr : conns : _ ->
      either throwString (parseAndSave scriptp (Char8.pack conns))
        . eitherDecode
        $ Lazy.Char8.pack pstr
    _ -> die "Usage ./parse <SCRIPT-PATH> <PID-MAP-JSON> <DB-STR>"

parseAndSave ::
  FilePath ->
  ByteString ->
  Map Ident (SingleOrMany PID, ScriptInputType) ->
  IO ()
parseAndSave p conns inputs = do
  t <- Text.IO.readFile p
  now <- fromIntegral @Int . round <$> getPOSIXTime
  ast <-
    either (throwString . displayException) pure . (`parse` t)
      =<< mkInferno @_ @BridgeMlValue (mkBridgePrelude funs) customTypes
  bracket (connectPostgreSQL conns) close (saveScriptAndParam ast now inputs)

saveScriptAndParam ::
  (Expr (Pinned VCObjectHash) (), TCScheme) ->
  CTime ->
  Map Ident (SingleOrMany PID, ScriptInputType) ->
  Connection ->
  IO ()
saveScriptAndParam x now inputs conn = insertScript *> insertParam
  where
    insertScript :: IO ()
    insertScript =
      void . withTransaction conn . execute conn q $
        InferenceScript hash vcmeta
      where
        q :: Query
        q = [sql| INSERT INTO scripts (id, obj) VALUES (?, ?) |]

    insertParam :: IO ()
    insertParam =
      void
        . withTransaction conn
        . execute conn q
        . InferenceParam
          Nothing
          hash
          -- Bit of a hack. We only have one model version in the
          -- tests, so we can just hard-code the ID here
          (Vector.singleton (Id 1))
          inputs
          128
          Nothing
        $ entityIdFromInteger 0
      where
        q :: Query
        q = [sql| INSERT INTO params VALUES (?, ?, ?, ?, ?, ?, ?) |]

    vcfunc :: VCObject
    vcfunc = uncurry VCFunction x

    hash :: VCObjectHash
    hash = vcHash vcfunc

    vcmeta :: VCMeta VCObject
    vcmeta = VCMeta now smd gid "mnist" "A script" Init VCObjectPublic vcfunc

    smd :: ScriptMetadata
    smd = ScriptMetadata uid mempty mempty

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
parse Interpreter {parseAndInfer} =
  parseAndInfer >>> \case
    Left e -> Left $ SomeInfernoError e
    Right (x, t, _, _) -> Right (void x, t)

-- These are needed to parse the script, but do not need to do anything
funs :: BridgeFuns IO
funs = BridgeFuns notSupported notSupported notSupported notSupported
  where
    notSupported :: a
    notSupported = error "Not supported"
