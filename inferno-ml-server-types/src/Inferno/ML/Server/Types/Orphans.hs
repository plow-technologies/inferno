{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Inferno.ML.Server.Types.Orphans where

import Control.DeepSeq (NFData)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (String),
    withText,
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Data (Typeable)
import Data.IP (IPv4)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple
  ( Binary (Binary),
    ResultError (ConversionFailed, UnexpectedNull),
  )
import Database.PostgreSQL.Simple.FromField
  ( Conversion,
    Field,
    FromField (fromField),
    returnError,
  )
import Database.PostgreSQL.Simple.ToField
  ( Action (Escape, EscapeByteA),
    ToField (toField),
  )
import Foreign.C (CTime (CTime))
import Inferno.Types.VersionControl
  ( VCObjectHash,
    byteStringToVCObjectHash,
    vcObjectHashToByteString,
  )
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
  fromField = maybeConversion $ readMaybe @IPv4 . ByteString.Char8.unpack

instance ToField IPv4 where
  toField = Escape . ByteString.Char8.pack . show

deriving anyclass instance NFData IPv4

instance FromField VCObjectHash where
  fromField f = \case
    Nothing -> returnError UnexpectedNull f "Expected non-empty bytea"
    Just bs ->
      fromField @(Binary ByteString) f (Just bs) >>= \case
        Binary b | Just h <- byteStringToVCObjectHash b -> pure h
        _ -> returnError ConversionFailed f "Invalid hash"

instance ToField VCObjectHash where
  toField = EscapeByteA . vcObjectHashToByteString

maybeConversion ::
  Typeable b => (a -> Maybe b) -> Field -> Maybe a -> Conversion b
maybeConversion f fld =
  maybe (returnError UnexpectedNull fld mempty) $
    maybe (returnError ConversionFailed fld mempty) pure
      . f
