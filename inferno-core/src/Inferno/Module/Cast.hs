{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- TODO export only needed?
-- module Inferno.Module.Cast (FromValue, ToValue) where
module Inferno.Module.Cast where

import Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import Control.Monad.Reader (ask)
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable, typeRep)
import Data.Word (Word16, Word32, Word64)
import Foreign.C.Types (CTime (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Inferno.Eval.Error (EvalError (CastError, NotFoundInImplicitEnv))
import Inferno.Module.Builtin (enumBoolHash)
import Inferno.Types.Syntax (ExtIdent (..), Lit (..), TList (..))
import Inferno.Types.Type (BaseType (..), InfernoType (..))
import Inferno.Types.Value (ImplEnvM, ImplicitCast (..), Value (..))
import Inferno.Utils.Prettyprinter (renderPretty)
import Prettyprinter (Pretty)

type Either3 a b c = Either a (Either b c)

type Either4 a b c d = Either a (Either3 b c d)

type Either5 a b c d e = Either a (Either4 b c d e)

type Either6 a b c d e f = Either a (Either5 b c d e f)

type Either7 a b c d e f g = Either a (Either6 b c d e f g)

-- | Types that can be converted to script values.
class ToValue c m a where
  toValue :: a -> Value c m

-- | Class of types that can be converted from script values, allowing IO in the process.
class FromValue c m a where
  fromValue :: MonadThrow m => (Value c m) -> m a

-- | Haskell types that can be casted to mask script types.
class Kind0 a where
  toType :: Proxy a -> InfernoType

-- Instances

couldNotCast :: forall c m a. (Pretty c, MonadThrow m, Typeable a) => Value c m -> m a
couldNotCast v =
  throwM $
    CastError $
      "Could not cast value "
        <> (unpack $ renderPretty v)
        <> " to "
        <> (show $ typeRep (Proxy :: Proxy a))

instance ToValue c m (Value c m) where
  toValue = id

instance FromValue c m (Value c m) where
  fromValue = pure

instance ToValue c m Lit where
  toValue l = case l of
    LInt i -> VInt i
    LDouble x -> VDouble x
    LText t -> VText t
    LHex w -> VWord64 w

instance ToValue c m Bool where
  toValue True = VEnum enumBoolHash "true"
  toValue False = VEnum enumBoolHash "false"

instance Pretty c => FromValue c m Bool where
  fromValue (VEnum hash ident) =
    if hash == enumBoolHash
      then
        if ident == "true"
          then pure True
          else pure False
      else couldNotCast $ (VEnum hash ident :: Value c m)
  fromValue v = couldNotCast v

instance ToValue c m Double where
  toValue = VDouble

instance Pretty c => FromValue c m Double where
  fromValue (VDouble x) = pure x
  -- fromValue (VInt x) = pure $ fromIntegral x
  fromValue v = couldNotCast v

instance ToValue c m Int64 where
  toValue = VInt

instance Pretty c => FromValue c m Int64 where
  fromValue (VInt x) = pure x
  fromValue v = couldNotCast v

instance ToValue c m Int where
  toValue = toValue . (fromIntegral :: Int -> Int64)

instance (Pretty c) => FromValue c m Int where
  fromValue v@(VInt i) =
    if (i :: Int64) < fromIntegral (minBound :: Int) || i > fromIntegral (maxBound :: Int)
      then couldNotCast v
      else pure $ fromIntegral i
  fromValue v = couldNotCast v

instance ToValue c m Integer where
  toValue = VInt . fromInteger

instance Pretty c => FromValue c m Integer where
  fromValue (VInt x) = pure $ fromIntegral x
  fromValue v = couldNotCast v

instance ToValue c m Word16 where
  toValue = VWord16

instance Pretty c => FromValue c m Word16 where
  fromValue (VWord16 w) = pure w
  fromValue v = couldNotCast v

instance ToValue c m Word32 where
  toValue = VWord32

instance Pretty c => FromValue c m Word32 where
  fromValue (VWord32 w) = pure w
  fromValue v = couldNotCast v

instance ToValue c m Word64 where
  toValue = VWord64

instance Pretty c => FromValue c m Word64 where
  fromValue (VWord64 w) = pure w
  fromValue v = couldNotCast v

instance ToValue c m () where
  toValue _ = VTuple []

instance Pretty c => FromValue c m () where
  fromValue (VTuple []) = pure ()
  fromValue v = couldNotCast v

instance ToValue c m CTime where
  toValue = VEpochTime

instance Pretty c => FromValue c m CTime where
  fromValue (VEpochTime t) = pure t
  fromValue v = couldNotCast v

instance ToValue c m Text where
  toValue = VText

instance Pretty c => FromValue c m Text where
  fromValue (VText t) = pure t
  fromValue v = couldNotCast v

instance Kind0 Bool where
  toType _ = TBase $ TEnum "bool" $ Set.fromList ["true", "false"]

instance Kind0 Float where
  toType _ = TBase $ TDouble

instance Kind0 Double where
  toType _ = TBase $ TDouble

instance Kind0 Int where
  toType _ = TBase $ TInt

instance Kind0 Int64 where
  toType _ = TBase $ TInt

instance Kind0 Integer where
  toType _ = TBase $ TInt

instance Kind0 Word16 where
  toType _ = TBase $ TWord16

instance Kind0 Word32 where
  toType _ = TBase $ TWord32

instance Kind0 Word64 where
  toType _ = TBase $ TWord64

instance Kind0 () where
  toType _ = TTuple TNil

instance Kind0 CTime where
  toType _ = TBase $ TTime

instance Kind0 Text where
  toType _ = TBase $ TText

instance (Kind0 a, Kind0 b) => Kind0 (a -> b) where
  toType _ = TArr (toType (Proxy :: Proxy a)) (toType (Proxy :: Proxy b))

instance (Kind0 a) => Kind0 [a] where
  toType _ = TArray (toType (Proxy :: Proxy a))

instance (MonadThrow m, FromValue c m a, ToValue c m b) => ToValue c m (a -> b) where
  toValue f =
    VFun $ \v -> do
      x <- fromValue v
      pure $ toValue $ f x

-- We have a separate instance for functions that run in the monad m.
-- This is because we cannot `toValue` the result, as it is impossible to have
-- toValue :: ImplEnvM m c (Value c (ImplEnvM m c)) -> Value c (ImplEnvM m c)
-- as that would entail running the ImplEnvM monad.
instance {-# OVERLAPPING #-} (MonadThrow m, FromValue c m a) => ToValue c m (a -> m (Value c m)) where
  toValue f =
    VFun $ \v -> do
      x <- fromValue v
      f x

instance (MonadThrow m, FromValue c (ImplEnvM m c) a1, ToValue c (ImplEnvM m c) (a2 -> a3), KnownSymbol lbl) => ToValue c (ImplEnvM m c) (ImplicitCast lbl a1 a2 a3) where
  toValue (ImplicitCast f) =
    VFun $ \b' -> do
      impl <- ask
      let i = ExtIdent $ Right $ pack $ symbolVal (Proxy :: Proxy lbl)
      case Map.lookup i impl of
        Just v -> do
          x <- fromValue v
          let f' = f x
          -- f' :: a2 -> a3, and a3 might be ImplEnvM m c (Value c (ImplEnvM m c)),
          -- so we cannot apply f' to (fromValue b') and toValue the result, see the above
          -- instance. Instead, we convert f' to a value and let the compiler pick the
          -- appropriate instance of ToValue c m (a -> b) from the 2 choices above.
          case toValue f' of
            VFun f'' ->
              f'' b'
            _ -> error ""
        Nothing -> throwM $ NotFoundInImplicitEnv i

-- | In this instance, the 'IO' in the type is ignored.
instance Kind0 a => Kind0 (IO a) where
  toType _ = toType (Proxy :: Proxy a)

instance ToValue c m a => ToValue c m (Maybe a) where
  toValue (Just x) = VOne $ toValue x
  toValue _ = VEmpty

instance (Typeable a, FromValue c m a, Pretty c) => FromValue c m (Maybe a) where
  fromValue VEmpty = pure Nothing
  fromValue (VOne v) = Just <$> fromValue v
  fromValue v = couldNotCast v

instance Kind0 a => Kind0 (Maybe a) where
  toType _ = TOptional (toType (Proxy :: Proxy a))

instance (ToValue c m a, ToValue c m b) => ToValue c m (Either a b) where
  toValue (Left x) = toValue x
  toValue (Right x) = toValue x

instance ToValue c m a => ToValue c m [a] where
  toValue xs = VArray $ map toValue xs

instance (Typeable a, FromValue c m a, Pretty c) => FromValue c m [a] where
  fromValue (VArray vs) = mapM fromValue vs
  fromValue v = couldNotCast v

instance (FromValue c m a, FromValue c m b, MonadCatch m) => FromValue c m (Either a b) where
  fromValue v = (Left <$> fromValue v) `catch` (\(_ :: EvalError) -> Right <$> fromValue v)

instance Kind0 (Either a b) where
  toType _ = error "Definitions with Either must have explicit type signature"

-- instance ToValue IO a => ToValue IO (IO a) where
--   toValue io = io >>= toValue

-- instance FromValue m a => FromValue m (IO a) where
--   fromValue = fmap pure . fromValue

-- instance FromValue m (EitherN '[]) where
--   fromValue v = undefined

-- instance (FromValue m a, FromValue m (EitherN as)) => FromValue m (EitherN (a ': as)) where
--   fromValue v = (Here <$> fromValue v) `catchError` (\_ -> Next <$> fromValue v)

-- instance ToValue m (EitherN '[]) where
--   toValue = undefined

-- instance (ToValue m a, ToValue m (EitherN as)) => ToValue m (EitherN (a ': as)) where
--   toValue (Here  x) = toValue x
--   toValue (Next x) = toValue x

-- serializeToDouble :: MonadThrow m => Env -> Value m' -> m Double
-- serializeToDouble TypeEnv{..} = \case
--   VInt i -> return $ fromIntegral i
--   VDouble d -> return d
--   VEnum "true" -> return 1.0
--   VEnum "false" -> return 0.0
--   VEnum e -> case Map.lookup e enums of
--     Just (EnumMeta _ _ cs _) -> case fromIntegral <$> elemIndex e cs of
--       Just d -> return d
--       Nothing -> throwM $ RuntimeError $ "Malformed environment! Could not find enum constructor in the list"
--     Just _ -> throwM $ RuntimeError $ "Malformed environment! Was expecting enum metadata"
--     Nothing -> throwM $ CastError $ "Enum #" <> Text.unpack e <> " could not be found in the environment."
--   VWord16 w -> return $ fromIntegral w
--   VWord32 w -> return $ fromIntegral w
--   VWord64 w -> return $ fromIntegral w

-- -- deserializeFromDouble :: MonadThrow m => Env -> Double -> InfernoType -> m (Value m')
-- -- deserializeFromDouble env d = \case
-- --   TBase TInt -> return $ VInt $
