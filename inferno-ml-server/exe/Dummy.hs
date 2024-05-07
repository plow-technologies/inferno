{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}

-- Dummy data server for "reading" and "writing" using the bridge. This permits
-- testing Inferno primitive evaluation in `inferno-ml-server` without needing
-- to have a real data source

module Dummy where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Inferno.ML.Server.Client.Bridge (api)
import Inferno.ML.Server.Module.Types (PID (PID))
import Lens.Micro.Platform
import Network.HTTP.Types (Status)
import Network.Wai (Request)
import Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    runSettings,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)
import Servant
import UnliftIO.Exception (throwIO, try)
import "inferno-ml-server-types" Inferno.ML.Server.Types
  ( BridgeAPI,
    IValue (IDouble, IEmpty),
  )

main :: IO ()
main = do
  putStrLn "Starting dummy bridge data server..."
  runServer
  where
    runServer :: IO ()
    runServer = withEnv $ run . dummyDataServer
      where
        run :: Application -> IO ()
        run app = withStdoutLogger $ (`runSettings` app) . mkSettings

    mkSettings :: (Request -> Status -> Maybe Integer -> IO ()) -> Settings
    mkSettings logger =
      defaultSettings
        & setPort 9999
        & setLogger logger

    dummyDataServer :: DummyEnv -> Application
    dummyDataServer env = serve api $ hoistServer api (`toHandler` env) server
      where
        toHandler :: DummyM a -> DummyEnv -> Handler a
        toHandler m = Handler . ExceptT . try . runReaderT m

    withEnv :: (DummyEnv -> IO ()) -> IO ()
    withEnv f = f . DummyEnv $ Map.fromList vals

    vals :: [(PID, Map Int Double)]
    vals =
      [ ( PID 1,
          Map.fromList [(150, 1.5), (250, 2.5)]
        ),
        ( PID 2,
          Map.fromList [(100, 10.0), (200, 20.0)]
        )
      ]

type DummyM = ReaderT DummyEnv IO

-- Dummy values to be looked up when the `valueAt` bridge is called
newtype DummyEnv = DummyEnv
  { values :: Map PID (Map Int Double)
  }
  deriving stock (Generic)

server :: ServerT (BridgeAPI PID Int) DummyM
server = valueAt :<|> latestValueAndTimeBefore :<|> valuesBetween

-- Dummy implementation of `valueAt`, ignoring resolution for now
valueAt :: Int64 -> PID -> Int -> DummyM IValue
valueAt _ p t =
  view #values
    <&> maybe IEmpty IDouble . preview (at p . _Just . at t . _Just)

latestValueAndTimeBefore :: Int -> PID -> DummyM IValue
latestValueAndTimeBefore _ _ = throwIO $ userError "Unsupported"

valuesBetween :: Int64 -> PID -> Int -> Int -> ReaderT DummyEnv IO IValue
valuesBetween _ _ _ _ = throwIO $ userError "Unsupported"
