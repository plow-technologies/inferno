{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Data.Proxy (Proxy (..))
import qualified Inferno.VersionControl.Operations.Filesystem as FSOps
import Inferno.VersionControl.Server (runServerConfig)
import Inferno.VersionControl.Server.Types (ServerConfig (..))
import Inferno.VersionControl.Testing (vcServerSpec)
import Servant.Client (BaseUrl (..), Scheme (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

main :: IO ()
main =
  withSystemTempDirectory "vc_store_" $ \vcPath -> do
    putStrLn $ "Store is at: " ++ (show vcPath)

    putStr "Starting Inferno VC..."
    _ <-
      forkIO $
        runServerConfig
          (Proxy :: Proxy Int)
          (Proxy :: Proxy Int)
          FSOps.withEnv
          FSOps.runInfernoVCFilesystemM
          ServerConfig
            { _serverHost = "127.0.0.1",
              _serverPort = 13077,
              _vcPath = vcPath
            }
    putStrLn "  Done."

    hspec $
      vcServerSpec
        BaseUrl
          { baseUrlScheme = Http,
            baseUrlHost = "127.0.0.1",
            baseUrlPort = 13077,
            baseUrlPath = []
          }
