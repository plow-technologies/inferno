{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- NOTE
-- This executable is only intended for testing the inference endpoint with the
-- `nixosTest` (see `../../../tests/server.nix`)

module Client (main) where

import Conduit
import Control.Monad (unless, void)
import Data.Coerce (coerce)
import qualified Data.Conduit.List as Conduit.List
import Data.Function (on)
import Data.Int (Int64)
import qualified Data.Map as Map
import Inferno.ML.Server.Client (inferenceC, registerBridgeC)
import Inferno.ML.Server.Types
  ( BridgeInfo (BridgeInfo),
    IValue (IDouble),
    Id (Id),
    WriteStream,
    toIPv4,
  )
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client.Streaming
  ( mkClientEnv,
    parseBaseUrl,
    runClientM,
    withClientM,
  )
import System.Exit (die)
import System.Posix.Types (EpochTime)
import Text.Read (readMaybe)
import UnliftIO (throwString)
import UnliftIO.Environment (getArgs)
import UnliftIO.Exception (throwIO)

main :: IO ()
main =
  getArgs >>= \case
    i : _ -> do
      ipid <- maybe (throwString "Invalid ID") (pure . Id) $ readMaybe i
      env <-
        mkClientEnv
          <$> newManager defaultManagerSettings
          <*> parseBaseUrl "http://localhost:8080"
      -- Register the bridge to communicate with the dummy bridge server
      void
        . flip runClientM env
        . registerBridgeC
        . flip BridgeInfo 9999
        $ toIPv4 (127, 0, 0, 1)
      withClientM (inferenceC ipid Nothing) env . either throwIO $
        verifyWrites (coerce ipid)
    _ -> die "Usage: test-client <inference-parameter-id>"

-- Check that the returned write stream matches the expected value
verifyWrites ::
  Int64 ->
  WriteStream IO ->
  IO ()
verifyWrites ipid c = do
  expected <- getExpected
  result <- rebuildWrites
  unless (result == expected) . throwString . unwords $
    [ "Expected: ",
      show expected,
      "but got:",
      show result,
      "for param",
      show ipid
    ]
  where
    rebuildWrites :: IO [(Int, [(EpochTime, IValue)])]
    rebuildWrites =
      runConduit $
        c
          .| Conduit.List.groupBy ((==) `on` fst)
          .| Conduit.List.concat
          .| sinkList

    getExpected :: IO [(Int, [(EpochTime, IValue)])]
    getExpected =
      maybe (throwString "Missing PID") pure . Map.lookup ipid $
        Map.fromList
          [ (1, [(1, [(151, IDouble 2.5), (251, IDouble 3.5)])]),
            (2, [(2, [(300, IDouble 25.0)])]),
            (3, [(3, [(100, IDouble 7.0)])])
          ]
