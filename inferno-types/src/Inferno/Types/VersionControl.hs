{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Inferno.Types.VersionControl where

import Control.DeepSeq (NFData)
import Crypto.Hash (Context, Digest, digestFromByteString, hashFinalize, hashInit, hashUpdate)
import Crypto.Hash.Algorithms (SHA256)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, withText)
import qualified Data.Binary.Put as Binary
import Data.ByteArray (ByteArrayAccess, convert)
import Data.ByteArray.Pack (fill, putBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Char8 as Char8
import Data.Data (Data)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Int (Int32, Int64)
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Serialize
  ( Serialize,
    get,
    getByteString,
    put,
    putByteString,
    runGet,
    runPut,
  )
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word32, Word64)
import Foreign.C.Types (CTime (..))
import GHC.Generics (C1, Constructor, D1, Generic, K1 (..), M1 (..), Rec1 (..), Rep, S1, U1, conName, from, (:*:) (..), (:+:) (..))
import Inferno.Types.Syntax
  ( Comment (..),
    Expr (..),
    ExtIdent (..),
    Fixity,
    IStr (..),
    Ident (..),
    ImplExpl (..),
    Import (..),
    InfixFixity,
    Lit (..),
    ModuleName (..),
    Pat (..),
    Scoped (..),
    SomeIStr (..),
    TList,
    tListToList,
  )
import Inferno.Types.Type
  ( BaseType (..),
    ImplType (..),
    InfernoType (..),
    Namespace,
    RestOfRecord (..),
    TCScheme (..),
    TV (..),
    TypeClass (..),
    TypeMetadata (..),
  )
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Text.Megaparsec (SourcePos)

newtype VCObjectHash = VCObjectHash {vcObjectHashDigest :: Digest SHA256}
  deriving stock (Generic, Data)
  deriving anyclass (ToJSONKey, FromJSONKey, NFData)
  deriving newtype (Eq, Ord, Read, ByteArrayAccess)

instance Show VCObjectHash where
  show = Char8.unpack . vcObjectHashToByteString

instance ToJSON VCObjectHash where
  toJSON = toJSON . decodeUtf8 . vcObjectHashToByteString

instance FromJSON VCObjectHash where
  parseJSON = withText "VCObjectHash" $ \piece -> do
    b64 <- either fail pure $ Base64.decode $ encodeUtf8 piece
    maybe (fail $ unpack $ "Cannot decode hash " <> piece) (pure . VCObjectHash) . digestFromByteString $ b64

instance Hashable VCObjectHash where
  hashWithSalt salt (VCObjectHash digest) =
    hashWithSalt salt $ convert @(Digest SHA256) @ByteString digest

instance Serialize VCObjectHash where
  get =
    getByteString 44
      >>= ( \b -> do
              b64 <- either fail pure $ Base64.decode b
              digest <- maybe (fail "VCObjectHash: Unable to digest from Base64 ByteString") pure $ digestFromByteString b64
              pure $ VCObjectHash digest
          )
  put = putByteString . Base64.encode . convert . vcObjectHashDigest

-- | Typeclass of hashable objects
class VCHashUpdate obj where
  (&<) :: Context SHA256 -> obj -> Context SHA256
  default (&<) :: (Generic obj, GenericVCHashUpdate (Rep obj)) => Context SHA256 -> obj -> Context SHA256
  ctxt &< o = genHashUpdate ctxt $ from o

hashUpdateVia :: (ByteArrayAccess ba) => (obj -> ba) -> Context SHA256 -> obj -> Context SHA256
hashUpdateVia toBAA ctxt obj = ctxt `hashUpdate` toBAA obj
{-# INLINE hashUpdateVia #-}

newtype VCHashUpdateViaShow a = VCHashUpdateViaShow {unVCHashUpdateViaShow :: a}

instance (Show a) => VCHashUpdate (VCHashUpdateViaShow a) where
  (&<) = hashUpdateVia $ Char8.pack . show . unVCHashUpdateViaShow

deriving via (VCHashUpdateViaShow ()) instance VCHashUpdate ()

deriving via (VCHashUpdateViaShow Char) instance VCHashUpdate Char

deriving via (VCHashUpdateViaShow Int) instance VCHashUpdate Int

deriving via (VCHashUpdateViaShow InfixFixity) instance VCHashUpdate InfixFixity

deriving via (VCHashUpdateViaShow Fixity) instance VCHashUpdate Fixity

instance VCHashUpdate CTime where
  ctxt &< (CTime t) = ctxt &< ("CTime" :: ByteString) &< t

instance (VCHashUpdate a) => VCHashUpdate (Maybe a) where
  ctxt &< Nothing = ctxt
  ctxt &< (Just o) = ctxt &< o

instance (VCHashUpdate a, VCHashUpdate b) => VCHashUpdate (a, b) where
  ctxt &< (a, b) = ctxt &< a &< b

instance (VCHashUpdate a, VCHashUpdate b, VCHashUpdate c) => VCHashUpdate (a, b, c) where
  ctxt &< (a, b, c) = ctxt &< a &< b &< c

instance (VCHashUpdate a, VCHashUpdate b, VCHashUpdate c, VCHashUpdate d) => VCHashUpdate (a, b, c, d) where
  ctxt &< (a, b, c, d) = ctxt &< a &< b &< c &< d

instance (VCHashUpdate a, VCHashUpdate b, VCHashUpdate c, VCHashUpdate d, VCHashUpdate e) => VCHashUpdate (a, b, c, d, e) where
  ctxt &< (a, b, c, d, e) = ctxt &< a &< b &< c &< d &< e

instance VCHashUpdate ByteString where
  (&<) = hashUpdate

instance VCHashUpdate Text where
  ctxt &< t = ctxt `hashUpdate` encodeUtf8 t

instance (VCHashUpdate a) => VCHashUpdate [a] where
  ctxt &< [] = ctxt &< ("[]" :: ByteString)
  ctxt &< (o : os) = ctxt &< (":" :: ByteString) &< o &< os

instance VCHashUpdate ExtIdent where
  ctxt &< (ExtIdent (Left a)) = ctxt &< ("var$" :: ByteString) &< a
  ctxt &< (ExtIdent (Right b)) = ctxt &< ("reg$" :: ByteString) &< b

instance (VCHashUpdate a) => VCHashUpdate (NonEmpty.NonEmpty a) where
  ctxt &< xs = ctxt &< NonEmpty.toList xs

instance (VCHashUpdate a) => VCHashUpdate (Set.Set a) where
  ctxt &< xs = ctxt &< Set.toList xs

instance (VCHashUpdate k, VCHashUpdate a) => VCHashUpdate (Map.Map k a) where
  ctxt &< m = ctxt &< Map.toList m

instance (VCHashUpdate a) => VCHashUpdate (IntMap.IntMap a) where
  ctxt &< m = ctxt &< IntMap.toList m

class GenericVCHashUpdate f where
  genHashUpdate :: Context SHA256 -> f p -> Context SHA256

instance GenericVCHashUpdate U1 where
  genHashUpdate ctxt _ = ctxt

instance (VCHashUpdate a) => GenericVCHashUpdate (K1 i a) where
  genHashUpdate ctxt (K1 x) = ctxt &< x

instance (GenericVCHashUpdate f) => GenericVCHashUpdate (D1 c f) where
  genHashUpdate ctxt (M1 x) = genHashUpdate ctxt x

instance (Constructor c, GenericVCHashUpdate f) => GenericVCHashUpdate (C1 c f) where
  genHashUpdate ctxt x@(M1 y) = ctxt &< Char8.pack (conName x) `genHashUpdate` y

instance (GenericVCHashUpdate f) => GenericVCHashUpdate (S1 c f) where
  genHashUpdate ctxt (M1 x) = genHashUpdate ctxt x

instance (GenericVCHashUpdate a, GenericVCHashUpdate b) => GenericVCHashUpdate (a :+: b) where
  genHashUpdate ctxt = \case
    L1 a -> genHashUpdate ctxt a
    R1 b -> genHashUpdate ctxt b

instance (GenericVCHashUpdate a, GenericVCHashUpdate b) => GenericVCHashUpdate (a :*: b) where
  genHashUpdate ctxt (a :*: b) = ctxt `genHashUpdate` a `genHashUpdate` b

instance (GenericVCHashUpdate f) => GenericVCHashUpdate (Rec1 f) where
  genHashUpdate ctxt (Rec1 a) = genHashUpdate ctxt a

deriving newtype instance VCHashUpdate Ident

deriving newtype instance VCHashUpdate ModuleName

instance VCHashUpdate VCObjectHash where
  ctxt &< (VCObjectHash h) = ctxt `hashUpdate` h

deriving instance VCHashUpdate ImplExpl

instance VCHashUpdate Int64 where
  (&<) = hashUpdateViaBinary Binary.putInt64le

instance VCHashUpdate Int32 where
  (&<) = hashUpdateViaBinary Binary.putInt32le

instance VCHashUpdate Double where
  (&<) = hashUpdateViaBinary Binary.putDoublele

instance VCHashUpdate Word32 where
  (&<) = hashUpdateViaBinary Binary.putWord32le

instance VCHashUpdate Word64 where
  (&<) = hashUpdateViaBinary Binary.putWord64le

hashUpdateViaBinary ::
  (t -> Binary.Put) ->
  Context SHA256 ->
  t ->
  Context SHA256
hashUpdateViaBinary p = hashUpdateVia (\d -> either error (id :: ByteString -> ByteString) $ let b = Char8.toStrict (Binary.runPut (p d)) in fill (Char8.length b) (putBytes b))

deriving instance VCHashUpdate Lit

instance (VCHashUpdate e) => VCHashUpdate (SomeIStr e) where
  ctxt &< (SomeIStr istr) = ctxt &< istr

instance (VCHashUpdate e) => VCHashUpdate (IStr f e) where
  ctxt &< istr = case istr of
    ISEmpty -> ctxt &< ("ISEmpty" :: ByteString)
    ISStr s is -> ctxt &< ("ISStr" :: ByteString) &< s &< is
    ISExpr e is -> ctxt &< ("ISExpr" :: ByteString) &< e &< is

deriving instance (VCHashUpdate a) => VCHashUpdate (Comment a)

instance (VCHashUpdate a) => VCHashUpdate (TList a) where
  ctxt &< ts = ctxt &< tListToList ts

deriving instance (VCHashUpdate a) => VCHashUpdate (Import a)

deriving instance (VCHashUpdate hash, VCHashUpdate a) => VCHashUpdate (Pat hash a)

deriving instance (VCHashUpdate a) => VCHashUpdate (Scoped a)

deriving instance (VCHashUpdate hash, VCHashUpdate a) => VCHashUpdate (Expr hash a)

deriving instance VCHashUpdate BaseType

deriving newtype instance VCHashUpdate TV

deriving instance VCHashUpdate RestOfRecord

deriving instance VCHashUpdate InfernoType

deriving instance VCHashUpdate TypeClass

deriving instance (VCHashUpdate ty) => VCHashUpdate (TypeMetadata ty)

deriving instance VCHashUpdate ImplType

deriving instance VCHashUpdate Namespace

deriving instance VCHashUpdate TCScheme

deriving via (VCHashUpdateViaShow SourcePos) instance VCHashUpdate SourcePos

data Pinned a = Local | Builtin VCObjectHash | UnderVC a
  deriving (Show, Eq, Ord, Generic, Functor, Data, ToJSON, FromJSON, VCHashUpdate)

pinnedToMaybe :: Pinned VCObjectHash -> Maybe VCObjectHash
pinnedToMaybe = \case
  Local -> Nothing
  Builtin h -> Just h
  UnderVC h -> Just h

pinnedUnderVCToMaybe :: Pinned a -> Maybe a
pinnedUnderVCToMaybe = \case
  UnderVC h -> Just h
  _ -> Nothing

vcObjectHashToByteString :: VCObjectHash -> ByteString
vcObjectHashToByteString = runPut . put

byteStringToVCObjectHash :: ByteString -> Maybe VCObjectHash
byteStringToVCObjectHash bs =
  let result :: Either String VCObjectHash
      result = runGet get bs
   in either (const Nothing) Just result

vcHash :: (VCHashUpdate obj) => obj -> VCObjectHash
vcHash o = VCObjectHash $ hashFinalize $ hashInit &< o

instance FromHttpApiData VCObjectHash where
  parseUrlPiece t = maybe (Left $ "Cannot decode hash " <> t) Right $ byteStringToVCObjectHash $ encodeUtf8 t

instance ToHttpApiData VCObjectHash where
  toUrlPiece = decodeUtf8 . vcObjectHashToByteString
