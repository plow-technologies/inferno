{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- NOTE
-- This executable is only intended for testing the inference endpoint with the
-- `nixosTest` (see `../../../tests/server.nix`)

module Client (main) where

import Conduit
import Control.Monad (unless, void)
import Data.Coerce (coerce)
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Sequence (Seq ((:|>)), (|>))
import qualified Data.Sequence as Sequence
import Inferno.ML.Server.Client (inferenceC, registerBridgeC)
import Inferno.ML.Server.Types
  ( BridgeInfo (BridgeInfo),
    IValue (IDouble),
    Id (Id),
    WriteStream,
    WriteStreamItem (WritePid, WriteValue),
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
    -- FIXME The reconstruction of the nested write structure should really
    -- be done inside the conduit
    rebuildWrites :: IO (Seq (Int, Seq (EpochTime, IValue)))
    rebuildWrites = fmap groupWrites . runConduit $ c .| sinkList

    getExpected :: IO (Seq (Int, Seq (EpochTime, IValue)))
    getExpected =
      maybe (throwString "") pure . Map.lookup ipid $
        Map.fromList
          [ ( 1,
              Sequence.fromList
                [ ( 1,
                    Sequence.fromList
                      [ (151, IDouble 2.5),
                        (251, IDouble 3.5)
                      ]
                  )
                ]
            ),
            ( 2,
              Sequence.fromList
                [ (2, Sequence.fromList [(300, IDouble 25.0)])
                ]
            ),
            ( 3,
              Sequence.fromList
                [ (3, Sequence.fromList [(100, IDouble 7.0)])
                ]
            )
          ]

groupWrites :: [WriteStreamItem] -> Seq (Int, Seq (EpochTime, IValue))
groupWrites xs = go xs Nothing mempty
  where
    go ::
      [WriteStreamItem] ->
      Maybe Int ->
      Seq (Int, Seq (EpochTime, IValue)) ->
      Seq (Int, Seq (EpochTime, IValue))
    go [] _ acc = acc
    go (WritePid p : ws) _ acc = go ws (Just p) $ acc |> (p, mempty)
    go (WriteValue _ : ws) Nothing acc = go ws Nothing acc
    go (WriteValue v : ws) (Just p) acc = case acc of
      rest :|> (p', vs)
        | p == p' -> go ws (Just p) $ rest |> (p, vs |> v)
      _ -> go ws (Just p) acc
