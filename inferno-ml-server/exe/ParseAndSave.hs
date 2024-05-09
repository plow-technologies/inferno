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
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Vector (Vector)
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
import Text.Read (readMaybe)
import UnliftIO.Exception (bracket, throwString)

main :: IO ()
main =
  getArgs >>= \case
    scriptp : p : conns : _ ->
      maybe
        (throwString "Invalid PID")
        (parseAndSave scriptp (Char8.pack conns) . PID)
        $ readMaybe p
    _ -> die "Usage ./parse <SCRIPT-PATH> <PID> <DB-STR>"

parseAndSave :: FilePath -> ByteString -> PID -> IO ()
parseAndSave p conns pid = do
  t <- Text.IO.readFile p
  now <- fromIntegral @Int . round <$> getPOSIXTime
  ast <-
    either (throwString . displayException) pure . (`parse` t)
      =<< mkInferno @_ @BridgeMlValue (mkBridgePrelude funs) customTypes
  bracket (connectPostgreSQL conns) close (saveScriptAndParam ast now pid)

saveScriptAndParam ::
  (Expr (Pinned VCObjectHash) (), TCScheme) ->
  CTime ->
  PID ->
  Connection ->
  IO ()
saveScriptAndParam x now pid conn = insertScript *> insertParam
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
          (Id 1)
          inputs
          mempty
          Nothing
        $ entityIdFromInteger 0
      where
        q :: Query
        q = [sql| INSERT INTO params VALUES (?, ?, ?, ?, ?, ?, ?) |]

        inputs :: Vector (SingleOrMany PID)
        inputs = Vector.singleton $ Single pid

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
