{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inferno.Module.Prelude.Defs where

import Control.Monad (foldM, when)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (bimap)
import Data.Bits
  ( clearBit,
    complement,
    complementBit,
    setBit,
    shift,
    testBit,
    unsafeShiftL,
    unsafeShiftR,
    xor,
    (.&.),
    (.|.),
  )
import qualified Data.ByteString as ByteString
import Data.Foldable (Foldable (foldl'), foldrM, maximumBy, minimumBy)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.List.Extra ((!?))
import Data.Ord (comparing)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Data.Time.Calendar (Day, addGregorianMonthsClip, addGregorianYearsClip, fromGregorian, toGregorian)
import Data.Time.Clock (DiffTime, UTCTime (..), diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Data.Time.Format as Time.Format
import Data.Word (Word16, Word32, Word64)
import Debug.Trace (trace)
import Foreign.C.Types (CTime (..))
import Foreign.Marshal.Utils (fromBool)
import Inferno.Eval.Error (EvalError (RuntimeError))
import Inferno.Module.Builtin (enumBoolHash)
import Inferno.Module.Cast (Either3, Either4, Either5, Either6, FromValue (fromValue))
import Inferno.Types.Type (BaseType (..), InfernoType (..))
import Inferno.Types.Value (Value (..))
import Inferno.Utils.Prettyprinter (renderPretty)
import Numeric.Statistics.Median (median)
import Prettyprinter (Pretty)
import System.Posix.Types (EpochTime)
import System.Random (randomIO)

zeroVal :: Value c m
zeroVal = VInt 0

secondsFun, minutesFun, hoursFun, daysFun, weeksFun, monthsFun, yearsFun :: Int64 -> CTime
secondsFun = CTime . fromIntegral
minutesFun = timeMultiplier 60
hoursFun = timeMultiplier 3600
daysFun = timeMultiplier 86400
weeksFun = timeMultiplier 604800
monthsFun = timeMultiplier 2592000
yearsFun = timeMultiplier 31536000

hourFun, dayFun, monthFun, yearFun :: CTime -> CTime
hourFun = mapEpochAsUTC $ \(UTCTime d diff) -> UTCTime d $ hourDiffTime diff
dayFun = mapEpochAsUTC midnightUTCTime
monthFun = mapEpochAsUTC $ \utc -> mapUTCTimeDay truncateMonth $ midnightUTCTime utc
yearFun = mapEpochAsUTC $ \utc -> mapUTCTimeDay truncateYear $ midnightUTCTime utc

mapUTCTimeDay :: (Day -> Day) -> (UTCTime -> UTCTime)
mapUTCTimeDay f (UTCTime d diff) = UTCTime (f d) diff

mapEpochAsUTC :: (UTCTime -> UTCTime) -> (CTime -> CTime)
mapEpochAsUTC f = CTime . round . utcTimeToPOSIXSeconds . f . posixSecondsToUTCTime . realToFrac

midnightUTCTime :: UTCTime -> UTCTime
midnightUTCTime (UTCTime d _) = UTCTime d 0

-- | Truncate a 'DiffTime' to the beginning of the hour.
hourDiffTime :: DiffTime -> DiffTime
hourDiffTime t =
  -- Note: one second is 10^12 picoseconds
  let hourlength = 60 * 60 * 10 ^ (12 :: Int)
   in picosecondsToDiffTime $ hourlength * div (diffTimeToPicoseconds t) hourlength

truncateMonth :: Day -> Day
truncateMonth day =
  let (y, m, _) = toGregorian day
   in fromGregorian y m 1

truncateYear :: Day -> Day
truncateYear day =
  let (y, _, _) = toGregorian day
   in fromGregorian y 1 1

secondsBeforeFun, minutesBeforeFun, hoursBeforeFun, daysBeforeFun, weeksBeforeFun :: CTime -> Int64 -> CTime
secondsBeforeFun t i = t - secondsFun i
minutesBeforeFun t i = t - minutesFun i
hoursBeforeFun t i = t - hoursFun i
daysBeforeFun t i = t - daysFun i
weeksBeforeFun t i = t - weeksFun i

monthsBeforeFun, yearsBeforeFun :: CTime -> Integer -> CTime
monthsBeforeFun t m = advanceMonths (negate m) t
yearsBeforeFun t y = advanceYears (negate y) t

advanceMonths :: Integer -> EpochTime -> EpochTime
advanceMonths months = mapEpochAsUTC $ \(UTCTime d diff) -> UTCTime (addGregorianMonthsClip months d) diff

advanceYears :: Integer -> EpochTime -> EpochTime
advanceYears years = mapEpochAsUTC $ \(UTCTime d diff) -> UTCTime (addGregorianYearsClip years d) diff

timeIntervalFun :: CTime -> CTime -> CTime -> [CTime]
timeIntervalFun every from to = [from, from + every .. to]

timeMultiplier :: CTime -> Int64 -> CTime
timeMultiplier t m = t * fromIntegral m

timeToInt :: CTime -> Int64
timeToInt (CTime t) = fromIntegral t

formatTime :: CTime -> Text -> Text
formatTime t f =
  let t1 = posixSecondsToUTCTime $ realToFrac t
   in pack $ Time.Format.formatTime Time.Format.defaultTimeLocale (unpack f) t1

parseTimeFun :: forall c m. (MonadThrow m) => Value c m
parseTimeFun =
  VFun $ \case
    VText fmt ->
      pure . VFun $ \case
        VText input -> pure . maybe VEmpty (VOne . toEpochTime) $ parseTime fmt input
        _ -> throwM $ RuntimeError "parseTime: expecting a time input string"
    _ -> throwM $ RuntimeError "parseTime: expecting a time format string"
  where
    toEpochTime :: UTCTime -> Value c m
    toEpochTime = VEpochTime . CTime . round . utcTimeToPOSIXSeconds

    parseTime :: Text -> Text -> Maybe UTCTime
    parseTime =
      Time.Format.parseTimeM True Time.Format.defaultTimeLocale `on` Text.unpack

randomFun :: (MonadIO m) => Value c m
randomFun = VFun $ \_ -> VDouble <$> randomIO

keepSomesFun :: (MonadThrow m) => Value c m
keepSomesFun =
  VFun $ \case
    VArray xs ->
      pure $
        VArray $
          foldr
            ( \v vs -> case v of
                VOne a -> a : vs
                _ -> vs
            )
            []
            xs
    _ -> throwM $ RuntimeError "keepSomes: expecting an array"

foldlFun :: (MonadThrow m) => Value c m
foldlFun =
  VFun $ \case
    VFun f ->
      return $
        VFun $ \z -> return $
          VFun $ \case
            VArray xs ->
              foldM
                ( \acc x ->
                    f acc >>= \case
                      VFun f' -> f' x
                      _ -> throwM $ RuntimeError "reduce: expecting a function when folding"
                )
                z
                xs
            _ -> throwM $ RuntimeError "reduce: expecting an array in the third argument"
    _ -> throwM $ RuntimeError "reduce: expecting a function in the first argument"

foldrFun :: (MonadThrow m) => Value c m
foldrFun =
  VFun $ \case
    VFun f ->
      return $
        VFun $ \z -> return $
          VFun $ \case
            VArray xs ->
              foldrM
                ( \x acc ->
                    f x >>= \case
                      VFun f' -> f' acc
                      _ -> throwM $ RuntimeError "reduceRight: expecting a function when folding"
                )
                z
                xs
            _ -> throwM $ RuntimeError "reduceRight: expecting an array in the third argument"
    _ -> throwM $ RuntimeError "reduceRight: expecting a function in the first argument"

traceFun :: (Monad m, Pretty c) => (Value c m)
traceFun = VFun $ \msg -> trace ("TRACE: " <> unpack (renderPretty msg)) $ return idFun

idFun :: (Monad m) => (Value c m)
idFun = VFun $ \x -> return x

eqFun :: (Monad m, Eq c) => (Value c m)
eqFun = VFun $ \x -> return $ VFun $ \y -> return $ if x == y then VEnum enumBoolHash "true" else VEnum enumBoolHash "false"

neqFun :: (Monad m, Eq c) => (Value c m)
neqFun = VFun $ \x -> return $ VFun $ \y -> return $ if x == y then VEnum enumBoolHash "false" else VEnum enumBoolHash "true"

enumFromToInt64 :: Int64 -> Int64 -> [Int64]
enumFromToInt64 = enumFromTo

sumFun ::
  Either6 Double Int64 EpochTime Word16 Word32 Word64 ->
  Either6
    (Either Double Int64 -> Double)
    (Either Double Int64 -> Either Double Int64)
    (EpochTime -> EpochTime)
    (Word16 -> Word16)
    (Word32 -> Word32)
    (Word64 -> Word64)
sumFun =
  bimap (\x -> either (x +) ((+) x . fromIntegral)) $
    bimap (\i -> bimap ((+) $ fromIntegral i) (i +)) $
      bimap (+) $
        bimap (+) $
          bimap (+) (+)

divFun ::
  Either Double Int64 ->
  Either
    (Either Double Int64 -> Double)
    (Either Double Int64 -> Either Double Int64)
divFun =
  bimap (\x -> either (x /) ((/) x . fromIntegral)) (\i -> bimap ((/) $ fromIntegral i) (div i))

modFun :: Int64 -> Int64 -> Int64
modFun = mod

mulFun ::
  Either3 Double Int64 EpochTime ->
  Either3
    (Either Double Int64 -> Double)
    (Either3 Double Int64 EpochTime -> Either3 Double Int64 EpochTime)
    (Int64 -> EpochTime)
mulFun =
  bimap (\x -> either (x *) ((*) x . fromIntegral)) $
    bimap
      (\i -> bimap ((*) $ fromIntegral i) (bimap (i *) ((*) $ secondsFun i)))
      (\x -> (*) x . secondsFun)

subFun ::
  Either6 Double Int64 EpochTime Word16 Word32 Word64 ->
  Either6
    (Either Double Int64 -> Double)
    (Either Double Int64 -> Either Double Int64)
    (EpochTime -> EpochTime)
    (Word16 -> Word16)
    (Word32 -> Word32)
    (Word64 -> Word64)
subFun =
  bimap (\x -> either (x -) ((-) x . fromIntegral)) $
    bimap (\i -> bimap ((-) $ fromIntegral i) (i -)) $
      bimap (-) $
        bimap (-) $
          bimap (-) (-)

recipFun :: Double -> Double
recipFun = recip

powFun :: Either Int64 Double -> Either (Int64 -> Int64) (Double -> Double)
powFun = bimap (^) (**)

expFun :: Double -> Double
expFun = exp

lnFun :: Double -> Double
lnFun = log

logFun :: Double -> Double
logFun = logBase 10

logBaseFun :: Double -> Double -> Double
logBaseFun = logBase

sqrtFun :: Double -> Double
sqrtFun = sqrt

negateFun :: Either3 Int64 Double EpochTime -> Either3 Int64 Double EpochTime
negateFun = bimap negate (bimap negate negate)

absFun :: Either3 Int64 Double EpochTime -> Either3 Int64 Double EpochTime
absFun = bimap abs (bimap abs abs)

floorFun :: Either Double Int64 -> Int64
floorFun = either floor id

ceilingFun :: Either Double Int64 -> Int64
ceilingFun = either ceiling id

roundFun :: Either Double Int64 -> Int64
roundFun = either round id

roundToFun :: Int64 -> Double -> Double
roundToFun n x =
  let q = 10 ^ n
   in fromIntegral (round (x * q) :: Int64) / q

truncateFun :: Either Double Int64 -> Int64
truncateFun = either truncate id

truncateToFun :: Int64 -> Double -> Double
truncateToFun n x =
  let q = 10 ^ n
   in fromIntegral (truncate (x * q) :: Int64) / q

limitFun :: Double -> Double -> Double -> Double
limitFun l u = min u . max l

piFun :: Double
piFun = pi

sinFun :: Double -> Double
sinFun = sin

sinhFun :: Double -> Double
sinhFun = sinh

arcSinFun :: Double -> Double
arcSinFun = asin

cosFun :: Double -> Double
cosFun = cos

coshFun :: Double -> Double
coshFun = cosh

arcCosFun :: Double -> Double
arcCosFun = acos

tanFun :: Double -> Double
tanFun = tan

tanhFun :: Double -> Double
tanhFun = tanh

arcTanFun :: Double -> Double
arcTanFun = atan

intToDouble :: Int64 -> Double
intToDouble = fromIntegral

doubleToInt :: Double -> Int64
doubleToInt = truncate

-- random :: () -> IO Double -- TODO types?
-- random = const randomIO

gtFun :: Either3 Int64 Double EpochTime -> Either3 (Int64 -> Bool) (Double -> Bool) (EpochTime -> Bool)
gtFun = bimap (>) (bimap (>) (>))

geqFun :: Either3 Int64 Double EpochTime -> Either3 (Int64 -> Bool) (Double -> Bool) (EpochTime -> Bool)
geqFun = bimap (>=) (bimap (>=) (>=))

ltFun :: Either3 Int64 Double EpochTime -> Either3 (Int64 -> Bool) (Double -> Bool) (EpochTime -> Bool)
ltFun = bimap (<) (bimap (<) (<))

leqFun :: Either3 Int64 Double EpochTime -> Either3 (Int64 -> Bool) (Double -> Bool) (EpochTime -> Bool)
leqFun = bimap (<=) (bimap (<=) (<=))

minFun :: Either3 Int64 Double EpochTime -> Either3 (Int64 -> Int64) (Double -> Double) (EpochTime -> EpochTime)
minFun = bimap min (bimap min min)

maxFun :: Either3 Int64 Double EpochTime -> Either3 (Int64 -> Int64) (Double -> Double) (EpochTime -> EpochTime)
maxFun = bimap max (bimap max max)

arrayIndexOptFun :: (MonadIO m, MonadThrow m, Pretty c) => Value c m
arrayIndexOptFun =
  VFun $ \case
    VArray a -> pure $ VFun $ \v -> do
      i <- fromValue v
      when (i > 1000) $ liftIO $ putStrLn $ "WARNING: Inferno: large array indexing: " <> show i
      case a !? i of
        Just x -> pure $ VOne x
        Nothing -> pure VEmpty
    _ -> throwM $ RuntimeError "arrayIndexOptFun: expecting an array"

arrayIndexFun :: (MonadIO m, MonadThrow m, Pretty c) => Value c m
arrayIndexFun =
  VFun $ \case
    VArray a -> pure $ VFun $ \v -> do
      i <- fromValue v
      when (i > 1000) $ liftIO $ putStrLn $ "WARNING: Inferno: large array indexing: " <> show i
      case a !? i of
        Just x -> pure x
        Nothing -> throwM $ RuntimeError "Array index out of bounds"
    _ -> throwM $ RuntimeError "arrayIndexFun: expecting an array"

consFun :: (MonadThrow m) => Value c m
consFun =
  VFun $ \v ->
    pure $ VFun $ \case
      VArray vs -> pure $ VArray $ v : vs
      _ -> throwM $ RuntimeError "cons: expecting an array"

unconsFun :: (MonadThrow m) => Value c m
unconsFun =
  VFun $ \case
    VArray [] -> pure VEmpty
    VArray (v : vs) -> pure $ VOne $ VTuple [v, VArray vs]
    _ -> throwM $ RuntimeError "uncons: expecting an array"

reverseFun :: (MonadThrow m) => Value c m
reverseFun =
  VFun $ \case
    VArray vs -> pure $ VArray $ reverse vs
    _ -> throwM $ RuntimeError "reverse: expecting an array"

takeFun :: (MonadThrow m) => Value c m
takeFun =
  VFun $ \case
    VInt n -> pure . VFun $ \case
      VArray vs -> pure . VArray . flip take vs $ fromIntegral n
      _ -> throwM $ RuntimeError "take: expecting an array"
    _ -> throwM $ RuntimeError "take: expecting an int"

dropFun :: (MonadThrow m) => Value c m
dropFun =
  VFun $ \case
    VInt n -> pure . VFun $ \case
      VArray vs -> pure . VArray . flip drop vs $ fromIntegral n
      _ -> throwM $ RuntimeError "drop: expecting an array"
    _ -> throwM $ RuntimeError "drop: expecting an int"

filterFun :: forall c m. (MonadThrow m) => Value c m
filterFun =
  VFun $ \case
    VFun p ->
      pure . VFun $ \case
        VArray vs -> VArray <$> applyFilter p vs
        _ -> throwM $ RuntimeError "filter: expecting an array"
    _ -> throwM $ RuntimeError "filter: expecting a function"
  where
    applyFilter :: (Value c m -> m (Value c m)) -> [Value c m] -> m [Value c m]
    applyFilter _ [] = pure mempty
    applyFilter p (x : xs) =
      p x >>= \case
        VEnum h "true"
          | h == enumBoolHash -> (x :) <$> applyFilter p xs
        VEnum h "false"
          | h == enumBoolHash -> applyFilter p xs
        _ -> throwM $ RuntimeError "filter: expecting predicate to return a bool"

takeWhileFun :: (MonadThrow m) => Value c m
takeWhileFun =
  VFun $ \case
    VFun p ->
      pure $ VFun $ \case
        VArray vs -> VArray <$> takeWhile' p vs
        _ -> throwM $ RuntimeError "takeWhile: expecting an array"
    _ -> throwM $ RuntimeError "takeWhile: expecting a function"
  where
    takeWhile' _ [] = pure []
    takeWhile' p (x : xs) =
      p x >>= \case
        VEnum h "true" | h == enumBoolHash -> (x :) <$> takeWhile' p xs
        VEnum h "false" | h == enumBoolHash -> pure []
        _ -> throwM $ RuntimeError "takeWhile: expecting predicate to return a bool"

dropWhileFun :: (MonadThrow m) => Value c m
dropWhileFun =
  VFun $ \case
    VFun p ->
      pure $ VFun $ \case
        VArray vs -> VArray <$> dropWhile' p vs
        _ -> throwM $ RuntimeError "dropWhile: expecting an array"
    _ -> throwM $ RuntimeError "dropWhile: expecting a function"
  where
    dropWhile' _ [] = pure []
    dropWhile' p xs@(x : xs') =
      p x >>= \case
        VEnum h "true" | h == enumBoolHash -> dropWhile' p xs'
        VEnum h "false" | h == enumBoolHash -> pure xs
        _ -> throwM $ RuntimeError "dropWhile: expecting predicate to return a bool"

singletonFun :: (Monad m) => (Value c m)
singletonFun = VFun $ \v -> return $ VArray [v]

-- The following functions use Int and not Int64, but that should be fine
-- because they don't create ints, these are only argument types.

testBitFun :: Either3 Word16 Word32 Word64 -> Int -> Bool
testBitFun = either testBit (either testBit testBit)

setBitFun :: Either3 Word16 Word32 Word64 -> Either3 (Int -> Word16) (Int -> Word32) (Int -> Word64)
setBitFun = bimap setBit (bimap setBit setBit)

clearBitFun :: Either3 Word16 Word32 Word64 -> Either3 (Int -> Word16) (Int -> Word32) (Int -> Word64)
clearBitFun = bimap clearBit (bimap clearBit clearBit)

complementBitFun :: Either3 Word16 Word32 Word64 -> Either3 (Int -> Word16) (Int -> Word32) (Int -> Word64)
complementBitFun = bimap complementBit (bimap complementBit complementBit)

complementFun :: Either4 Bool Word16 Word32 Word64 -> Either4 Bool Word16 Word32 Word64
complementFun = bimap not (bimap complement (bimap complement complement))

andFun :: Either4 Bool Word16 Word32 Word64 -> Either4 (Bool -> Bool) (Word16 -> Word16) (Word32 -> Word32) (Word64 -> Word64)
andFun = bimap (&&) (bimap (.&.) (bimap (.&.) (.&.)))

orFun :: Either4 Bool Word16 Word32 Word64 -> Either4 (Bool -> Bool) (Word16 -> Word16) (Word32 -> Word32) (Word64 -> Word64)
orFun = bimap (||) (bimap (.|.) (bimap (.|.) (.|.)))

xorFun :: Either4 Bool Word16 Word32 Word64 -> Either4 (Bool -> Bool) (Word16 -> Word16) (Word32 -> Word32) (Word64 -> Word64)
xorFun = bimap xor (bimap xor (bimap xor xor))

shiftFun :: Either3 Word16 Word32 Word64 -> Either3 (Int -> Word16) (Int -> Word32) (Int -> Word64)
shiftFun = bimap shift (bimap shift shift)

toWord64Fun :: Either5 Bool Word16 Word32 Word64 Int64 -> Word64
toWord64Fun = either fromBool (either fromIntegral (either fromIntegral $ either id fromIntegral))

toWord32Fun :: Either5 Bool Word16 Word32 Word64 Int64 -> Word32
toWord32Fun = either fromBool (either fromIntegral (either id (either (fromIntegral . (.&.) 0xFFFFFFFF) fromIntegral)))

toWord16Fun :: Either5 Bool Word16 Word32 Word64 Int64 -> Word16
toWord16Fun = either fromBool (either id (either (fromIntegral . (.&.) 0xFFFF) (either (fromIntegral . (.&.) 0xFFFF) fromIntegral)))

fromWordFun :: Either4 Bool Word16 Word32 Word64 -> Int64
fromWordFun = either fromBool (either fromIntegral (either fromIntegral fromIntegral))

zeroFun :: (MonadThrow m) => (Value c m)
zeroFun = VFun $ \case
  VTypeRep (TBase TInt) -> return $ VInt 0
  VTypeRep (TBase TDouble) -> return $ VDouble 0
  VTypeRep (TBase TTimeDiff) -> return $ VEpochTime 0
  VTypeRep (TBase TWord16) -> return $ VWord16 0
  VTypeRep (TBase TWord32) -> return $ VWord32 0
  VTypeRep (TBase TWord64) -> return $ VWord64 0
  VTypeRep ty -> throwM $ RuntimeError $ "zeroFun: unexpected runtimeRep " <> show ty
  _ -> throwM $ RuntimeError "zeroFun: expecting a runtimeRep"

zipFun :: (MonadThrow m) => Value c m
zipFun = VFun $ \case
  VArray xs ->
    return $ VFun $ \case
      VArray ys ->
        return $ VArray $ zipWith (\v1 v2 -> VTuple [v1, v2]) xs ys
      _ -> throwM $ RuntimeError "zip: expecting an array"
  _ -> throwM $ RuntimeError "zip: expecting an array"

lengthFun :: (MonadThrow m) => Value c m
lengthFun =
  VFun $ \case
    VArray xs -> pure $ VInt $ fromIntegral $ length xs
    _ -> throwM $ RuntimeError "length: expecting an array"

extractInts :: (MonadThrow m) => [Value custom m] -> m [Int64]
extractInts = \case
  [] -> pure []
  VInt x : vs -> (x :) <$> extractInts vs
  _ -> throwM $ RuntimeError "extractInts: got an array with mixed types"

extractDoubles :: (MonadThrow m) => [Value custom m] -> m [Double]
extractDoubles = \case
  [] -> pure []
  VDouble x : vs -> (x :) <$> extractDoubles vs
  _ -> throwM $ RuntimeError "extractDoubles: got an array with mixed types"

extractEpochTimes :: (MonadThrow m) => [Value custom m] -> m [EpochTime]
extractEpochTimes = \case
  [] -> pure []
  VEpochTime x : vs -> (x :) <$> extractEpochTimes vs
  _ -> throwM $ RuntimeError "extractEpochTimes: got an array with mixed types"

minimumFun :: (MonadThrow m) => Value c m
minimumFun =
  VFun $ \case
    VArray [] -> pure VEmpty
    VArray vs@(VInt _ : _) -> VOne . VInt . minimum <$> extractInts vs
    VArray vs@(VDouble _ : _) -> VOne . VDouble . minimum <$> extractDoubles vs
    VArray vs@(VEpochTime _ : _) -> VOne . VEpochTime . minimum <$> extractEpochTimes vs
    VArray _ -> throwM $ RuntimeError "minimum: unsupported array type"
    _ -> throwM $ RuntimeError "minimum: expecting an array"

maximumFun :: (MonadThrow m) => Value c m
maximumFun =
  VFun $ \case
    VArray [] -> pure VEmpty
    VArray vs@(VInt _ : _) -> VOne . VInt . maximum <$> extractInts vs
    VArray vs@(VDouble _ : _) -> VOne . VDouble . maximum <$> extractDoubles vs
    VArray vs@(VEpochTime _ : _) -> VOne . VEpochTime . maximum <$> extractEpochTimes vs
    VArray _ -> throwM $ RuntimeError "maximum: unsupported array type"
    _ -> throwM $ RuntimeError "maximum: expecting an array"

averageFun :: (MonadThrow m) => Value c m
averageFun =
  VFun $ \case
    VArray [] -> pure VEmpty
    VArray vs@(VInt _ : _) -> VOne . VDouble . average . map fromIntegral <$> extractInts vs
    VArray vs@(VDouble _ : _) -> VOne . VDouble . average <$> extractDoubles vs
    VArray _ -> throwM $ RuntimeError "average: unsupported array type"
    _ -> throwM $ RuntimeError "average: expecting an array"
  where
    average :: (Foldable f, Fractional a) => f a -> a
    average xs
      | null xs = error "average: impossible"
      | otherwise =
          uncurry (/)
            . foldl' (\(!total, !count) x -> (total + x, count + 1)) (0, 0)
            $ xs

medianFun :: (MonadThrow m) => Value c m
medianFun =
  VFun $ \case
    VArray [] -> pure VEmpty
    VArray vs@(VInt _ : _) -> VOne . VDouble . median . map fromIntegral <$> extractInts vs
    VArray vs@(VDouble _ : _) -> VOne . VDouble . median <$> extractDoubles vs
    VArray _ -> throwM $ RuntimeError "median: unsupported array type"
    _ -> throwM $ RuntimeError "median: expecting an array"

argminFun :: (MonadThrow m) => Value c m
argminFun =
  VFun $ \case
    VArray [] -> pure VEmpty
    VArray vs@(VInt _ : _) -> VOne . VInt . argMin' <$> extractInts vs
    VArray vs@(VDouble _ : _) -> VOne . VInt . argMin' <$> extractDoubles vs
    VArray vs@(VEpochTime _ : _) -> VOne . VInt . argMin' <$> extractEpochTimes vs
    VArray _ -> throwM $ RuntimeError "argmin: unsupported array type"
    _ -> throwM $ RuntimeError "argmin: expecting an array"
  where
    argMin' :: (Ord a) => [a] -> Int64
    argMin' = fst . minimumBy (comparing snd) . zip [0 ..]

argmaxFun :: (MonadThrow m) => Value c m
argmaxFun =
  VFun $ \case
    VArray [] -> pure VEmpty
    VArray vs@(VInt _ : _) -> VOne . VInt . argMax' <$> extractInts vs
    VArray vs@(VDouble _ : _) -> VOne . VInt . argMax' <$> extractDoubles vs
    VArray vs@(VEpochTime _ : _) -> VOne . VInt . argMax' <$> extractEpochTimes vs
    VArray _ -> throwM $ RuntimeError "argmax: unsupported array type"
    _ -> throwM $ RuntimeError "argmax: expecting an array"
  where
    argMax' :: (Ord a) => [a] -> Int64
    argMax' = fst . maximumBy (comparing snd) . zip [0 ..]

argsortFun :: (MonadThrow m) => Value c m
argsortFun =
  VFun $ \case
    VArray [] -> pure VEmpty
    VArray vs@(VInt _ : _) -> VArray . argsort' <$> extractInts vs
    VArray vs@(VDouble _ : _) -> VArray . argsort' <$> extractDoubles vs
    VArray vs@(VEpochTime _ : _) -> VArray . argsort' <$> extractEpochTimes vs
    VArray _ -> throwM $ RuntimeError "argmax: unsupported array type"
    _ -> throwM $ RuntimeError "argsort: expecting an array"
  where
    argsort' :: (Ord a) => [a] -> [Value c m]
    argsort' xs = map (VInt . fst) $ sortOn snd $ zip [0 ..] xs

magnitudeFun :: (MonadThrow m) => Value c m
magnitudeFun =
  VFun $ \case
    VArray [] -> pure VEmpty
    VArray vs@(VInt _ : _) -> VOne . VDouble . magnitude . map fromIntegral <$> extractInts vs
    VArray vs@(VDouble _ : _) -> VOne . VDouble . magnitude <$> extractDoubles vs
    VArray _ -> throwM $ RuntimeError "magnitude: unsupported array type"
    _ -> throwM $ RuntimeError "magnitude: expecting a number"
  where
    magnitude = sqrt . sum . map (** 2)

normFun :: (MonadThrow m) => Value c m
normFun = magnitudeFun

appendText :: Text -> Text -> Text
appendText = Text.append

textLength :: Text -> Int64
textLength = fromIntegral . Text.length

stripText :: Text -> Text
stripText = Text.strip

toUpperText :: Text -> Text
toUpperText = Text.toUpper

toLowerText :: Text -> Text
toLowerText = Text.toLower

-- | Encode text as UTF-8 bytes. Returns an array of @Word16@ values representing
-- the UTF-8 byte sequence. Note: We use @Word16@ instead of @Word8@ because Inferno
-- does not support @Word8@, and it is not feasible to add support at this time.
encodeUtf8Text :: Text -> [Word16]
encodeUtf8Text =
  fmap fromIntegral . ByteString.unpack . Text.Encoding.encodeUtf8

-- | Decode UTF-8 bytes to text. Takes an array of @Word16@ values (each representing
-- a byte) and converts them to text. Uses lenient decoding to handle invalid UTF-8
-- sequences gracefully. Note: We use @Word16@ instead of @Word8@ because Inferno does
-- not support @Word8@, and it is not feasible to add support at this time.
decodeUtf8Text :: [Word16] -> Text
decodeUtf8Text =
  Text.Encoding.decodeUtf8Lenient . ByteString.pack . fmap fromIntegral

textSplitAt :: (MonadThrow m) => Value c m
textSplitAt =
  VFun $ \case
    VInt n -> pure $
      VFun $ \case
        VText txt ->
          let (a, b) = Text.splitAt (fromIntegral n) txt
           in pure $ VTuple [VText a, VText b]
        _ -> throwM $ RuntimeError "splitAt: expecting text for the second argument"
    _ -> throwM $ RuntimeError "splitAt: expecting an int for the first argument"

-- | Convert unsigned integer to binary-coded decimal.
toBCD :: Word -> Word
toBCD = digitSum 0 0
  where
    digitSum :: Word -> Int -> Word -> Word
    digitSum acc _ x | x == 0 = acc
    digitSum acc offset x =
      let (q, r) = divMod x 10
          acc' = unsafeShiftL r offset .|. acc
       in acc' `seq` digitSum acc' (offset + 4) q

toBCDFun :: Word64 -> Word64
toBCDFun = fromIntegral . toBCD . fromIntegral

-- | Convert binary-coded decimal to unsigned integer.
fromBCD :: Word -> Maybe Word
fromBCD = digitSeparation 0 1
  where
    digitSeparation :: Word -> Word -> Word -> Maybe Word
    digitSeparation acc _ x | x == 0 = Just acc
    digitSeparation acc offset x =
      let d = x .&. 0b1111
          acc' = acc + d * offset
       in if d <= 9
            then acc' `seq` digitSeparation acc' (offset * 10) (unsafeShiftR x 4)
            else Nothing

fromBCDFun :: Word64 -> Maybe Word64
fromBCDFun = fmap fromIntegral . fromBCD . fromIntegral
