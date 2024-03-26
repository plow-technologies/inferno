-- NOTE
-- This executable is only intended for testing the inference endpoint with the
-- `nixosTest` (see `../../../tests/server.nix`)

module Client (main) where

import Control.Monad (void)
import Inferno.ML.Server.Client (inferenceC, registerBridgeC)
import Inferno.ML.Server.Types (BridgeInfo (BridgeInfo), Id (Id), toIPv4)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client.Streaming
  ( mkClientEnv,
    parseBaseUrl,
    runClientM,
    withClientM,
  )
import System.Exit (die)
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
      -- Run the given inference param. The test scripts should use `writePairs`.
      -- The dummy bridge implementation will write this to a file for later
      -- inspection
      withClientM (inferenceC ipid Nothing) env . either throwIO . const $ pure ()
    _ -> die "Usage: test-client <inference-parameter-id>"
