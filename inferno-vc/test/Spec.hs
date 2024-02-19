{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent (forkIO)
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
    putStrLn $ "Store is at: " ++ show vcPath
    putStr "Starting Inferno VC..."
    _ <-
      forkIO $
        runServerConfig
          id
          FSOps.withEnv
          (FSOps.runInfernoVCFilesystemM @Int @Int)
          ServerConfig
            { serverHost = "127.0.0.1",
              serverPort = 13077,
              vcPath = vcPath
            }
    putStrLn "  Done."

    hspec $
      vcServerSpec @Int @Int
        BaseUrl
          { baseUrlScheme = Http,
            baseUrlHost = "127.0.0.1",
            baseUrlPort = 13077,
            baseUrlPath = []
          }
