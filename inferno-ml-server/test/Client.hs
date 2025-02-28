{-# LANGUAGE DataKinds #-}

-- NOTE
-- This executable is only intended for testing the inference endpoint with the
-- `nixosTest` (see `../../../tests/server.nix`)

module Client (main) where

import Conduit
import Control.Monad (unless)
import Data.Coerce (coerce)
import qualified Data.Map as Map
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Inferno.ML.Server.Client (inferenceC)
import Inferno.ML.Server.Types
  ( IValue (IDouble),
    Id (Id),
    WriteStream,
  )
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client.Streaming
  ( mkClientEnv,
    parseBaseUrl,
    withClientM,
  )
import System.Exit (die)
import System.Posix.Types (EpochTime)
import System.Random (randomIO)
import Text.Read (readMaybe)
import UnliftIO (throwString)
import UnliftIO.Environment (getArgs)
import UnliftIO.Exception (throwIO)

main :: IO ()
main =
  getArgs >>= \case
    i : _ -> do
      ipid <- maybe (throwString "Invalid ID") (pure . Id) $ readMaybe i
      uuid <- randomIO
      env <-
        mkClientEnv
          <$> newManager defaultManagerSettings
          <*> parseBaseUrl "http://localhost:8080"
      withClientM (inferenceC ipid Nothing uuid) env . either throwIO $
        verifyWrites (coerce ipid)
    _ -> die "Usage: test-client <inference-parameter-id>"

-- Check that the returned write stream matches the expected value
verifyWrites :: UUID -> WriteStream IO -> IO ()
verifyWrites ipid c = do
  expected <- getExpected
  -- Note that there are only one or two chunks per PID in the output stream, so
  -- we don't need to concatenate the results by PID. We can just sink it into
  -- a list directly
  result <- runConduit $ c .| sinkList
  unless (result == expected) . throwString . unwords $
    [ "Expected: "
    , show expected
    , "but got:"
    , show result
    , "for param"
    , show ipid
    ]
  where
    getExpected :: IO [(Int, [(IValue, EpochTime)])]
    getExpected =
      maybe (throwString "Missing output PID for parameter") pure
        . Map.lookup ipid
        $ Map.fromList
          [
            ( UUID.fromWords 1 0 0 0
            ,
              [ (1, [(IDouble 2.5, 151), (IDouble 3.5, 251)])
              ]
            )
          ,
            ( UUID.fromWords 2 0 0 0
            ,
              [ (2, [(IDouble 25.0, 300)])
              ]
            )
          ,
            ( UUID.fromWords 3 0 0 0
            ,
              [ (3, [(IDouble 7.0, 100)])
              , (4, [(IDouble 8.0, 100)])
              ]
            )
          ]
