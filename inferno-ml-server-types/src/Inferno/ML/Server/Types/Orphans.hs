{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Inferno.ML.Server.Types.Orphans where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (String),
    withText,
  )
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.IP (IPv4)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple
  ( ResultError (ConversionFailed, UnexpectedNull),
  )
import Database.PostgreSQL.Simple.FromField
  ( FromField (fromField),
    returnError,
  )
import Database.PostgreSQL.Simple.ToField
  ( Action (Escape),
    ToField (toField),
  )
import Foreign.C (CTime (CTime))
import System.Posix (EpochTime)
import Text.Read (readMaybe)
import Web.HttpApiData
  ( FromHttpApiData (parseUrlPiece),
    ToHttpApiData (toUrlPiece),
  )

deriving newtype instance ToHttpApiData EpochTime

deriving newtype instance FromHttpApiData EpochTime

instance FromJSON IPv4 where
  parseJSON =
    withText "IPv4" $
      maybe (fail "Invalid IPv4") pure
        . readMaybe
        . Text.unpack

instance ToJSON IPv4 where
  toJSON = String . Text.pack . show

instance FromHttpApiData IPv4 where
  parseUrlPiece =
    maybe (Left "Invalid IPv4") Right
      . readMaybe @IPv4
      . Text.unpack

instance ToHttpApiData IPv4 where
  toUrlPiece = Text.pack . show

instance FromField IPv4 where
  fromField f =
    maybe (returnError UnexpectedNull f mempty) $
      maybe (returnError ConversionFailed f mempty) pure
        . readMaybe @IPv4
        . ByteString.Char8.unpack

instance ToField IPv4 where
  toField = Escape . ByteString.Char8.pack . show
