-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DerivingStrategies #-}

module Main where

-- import Inferno.VersionControl.Server (runServer)

main :: IO ()
main = putStrLn "NOTE: the commented code in this file demonstrates how to instantiate Inferno with custom author and group types and obtain an executable."

-- deriving anyclass instance VCHashUpdate AuthorType

-- deriving anyclass instance VCHashUpdate GroupType

-- main :: IO ()
-- main = runServer (Proxy :: Proxy AuthorType) (Proxy :: Proxy GroupType)
