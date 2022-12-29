module Inferno.Module.Prelude.Defs where

import Control.Monad (foldM)
import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (bimap)
import Data.Bits
  ( clearBit,
    complement,
    complementBit,
    setBit,
    shift,
    testBit,
    xor,
    (.&.),
    (.|.),
  )
import Data.Foldable (foldrM, maximumBy, minimumBy)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
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
import Inferno.Module.Cast (Either3, Either4, Either5, Either6)
import Inferno.Types.Type (BaseType (..), InfernoType (..))
import Inferno.Types.Value (Value (..))
import Inferno.Utils.Prettyprinter (renderPretty)
import Prettyprinter (Pretty)
import System.Posix.Types (EpochTime)

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
secondsBeforeFun t i = t - (secondsFun i)
minutesBeforeFun t i = t - (minutesFun i)
hoursBeforeFun t i = t - (hoursFun i)
daysBeforeFun t i = t - (daysFun i)
weeksBeforeFun t i = t - (weeksFun i)

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

keepSomesFun :: (MonadError EvalError m) => Value c m
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
    _ -> throwError $ RuntimeError "keepSomes: expecting an array"

foldlFun :: (MonadError EvalError m) => Value c m
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
                      _ -> throwError $ RuntimeError "reduce: expecting a function when folding"
                )
                z
                xs
            _ -> throwError $ RuntimeError "reduce: expecting an array in the third argument"
    _ -> throwError $ RuntimeError "reduce: expecting a function in the first argument"

foldrFun :: (MonadError EvalError m) => Value c m
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
                      _ -> throwError $ RuntimeError "reduceRight: expecting a function when folding"
                )
                z
                xs
            _ -> throwError $ RuntimeError "reduceRight: expecting an array in the third argument"
    _ -> throwError $ RuntimeError "reduceRight: expecting a function in the first argument"

traceFun :: (Monad m, Pretty c) => (Value c m)
traceFun = VFun $ \msg -> trace ("TRACE: " <> unpack (renderPretty msg)) $ return idFun

idFun :: Monad m => (Value c m)
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
  bimap (\x -> either ((+) x) ((+) x . fromIntegral)) $
    bimap (\i -> bimap ((+) $ fromIntegral i) ((+) i)) $
      bimap (+) $ bimap (+) $ bimap (+) (+)

divFun ::
  Either Double Int64 ->
  Either
    (Either Double Int64 -> Double)
    (Either Double Int64 -> Either Double Int64)
divFun =
  bimap (\x -> either ((/) x) ((/) x . fromIntegral)) $
    (\i -> bimap ((/) $ fromIntegral i) ((div) i))

modFun :: Int64 -> Int64 -> Int64
modFun = mod

mulFun ::
  Either3 Double Int64 EpochTime ->
  Either3
    (Either Double Int64 -> Double)
    (Either3 Double Int64 EpochTime -> Either3 Double Int64 EpochTime)
    (Int64 -> EpochTime)
mulFun =
  bimap (\x -> either ((*) x) ((*) x . fromIntegral)) $
    bimap
      (\i -> bimap ((*) $ fromIntegral i) (bimap ((*) i) ((*) $ secondsFun i)))
      (\x -> ((*) x . secondsFun))

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
  bimap (\x -> either ((-) x) ((-) x . fromIntegral)) $
    bimap (\i -> bimap ((-) $ fromIntegral i) ((-) i)) $
      bimap (-) $ bimap (-) $ bimap (-) (-)

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
limitFun = (\l u -> min u . max l)

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
minFun = bimap (min) (bimap (min) (min))

maxFun :: Either3 Int64 Double EpochTime -> Either3 (Int64 -> Int64) (Double -> Double) (EpochTime -> EpochTime)
maxFun = bimap (max) (bimap (max) (max))

singletonFun :: Monad m => (Value c m)
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
xorFun = bimap (xor) (bimap (xor) (bimap (xor) (xor)))

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

zeroFun :: MonadError EvalError m => (Value c m)
zeroFun = VFun $ \case
  VTypeRep (TBase TInt) -> return $ VInt 0
  VTypeRep (TBase TDouble) -> return $ VDouble 0
  VTypeRep (TBase TTimeDiff) -> return $ VEpochTime 0
  VTypeRep (TBase TWord16) -> return $ VWord16 0
  VTypeRep (TBase TWord32) -> return $ VWord32 0
  VTypeRep (TBase TWord64) -> return $ VWord64 0
  VTypeRep ty -> throwError $ RuntimeError $ "zeroFun: unexpected runtimeRep " <> show ty
  _ -> throwError $ RuntimeError "zeroFun: expecting a runtimeRep"

lengthFun :: (MonadError EvalError m) => Value c m
lengthFun =
  VFun $ \case
    VArray xs -> pure $ VInt $ fromIntegral $ length xs
    _ -> throwError $ RuntimeError "length: expecting an array"

-- | Convenience function for comparing numbered value
-- in an array while maintaining the original value type
keepNumberValues :: [Value c m] -> [(Value c m, Double)]
keepNumberValues =
  mapMaybe
    ( \case
        m@(VInt v) -> Just (m, fromIntegral v)
        m@(VDouble v) -> Just (m, v)
        _ -> Nothing
    )

minimumFun :: (MonadError EvalError m) => Value c m
minimumFun =
  VFun $ \case
    VArray [] -> throwError $ RuntimeError "minimum: expecting a non-empty array"
    VArray xs -> return $ fst $ minimumBy (comparing snd) $ keepNumberValues xs
    _ -> throwError $ RuntimeError "minimum: expecting an array"

maximumFun :: (MonadError EvalError m) => Value c m
maximumFun =
  VFun $ \case
    VArray [] -> throwError $ RuntimeError "maximum: expecting a non-empty array"
    VArray xs -> return $ fst $ maximumBy (comparing snd) $ keepNumberValues xs
    _ -> throwError $ RuntimeError "maximum: expecting an array"

averageFun :: (MonadError EvalError m) => Value c m
averageFun =
  VFun $ \case
    VArray [] -> throwError $ RuntimeError "average: expecting a non-empty array"
    VArray xs -> return $ VDouble $ sum (mapMaybe toDouble xs) / fromIntegral (length xs)
    _ -> throwError $ RuntimeError "average: expecting an array"
  where
    toDouble :: Value c m -> Maybe Double
    toDouble = \case
      VInt v -> Just $ fromIntegral v
      VDouble v -> Just v
      _ -> Nothing

argminFun :: (MonadError EvalError m) => Value c m
argminFun =
  VFun $ \case
    VArray xs -> pure $ VInt $ fromIntegral $ argMin' $ map snd $ keepNumberValues xs
    _ -> throwError $ RuntimeError "argmin: expecting an array"
  where
    argMin' :: [Double] -> Int
    argMin' = fst . minimumBy (comparing snd) . zip [0 ..]

argmaxFun :: (MonadError EvalError m) => Value c m
argmaxFun =
  VFun $ \case
    VArray xs -> pure $ VInt $ fromIntegral $ argMax' $ map snd $ keepNumberValues xs
    _ -> throwError $ RuntimeError "argmax: expecting an array"
  where
    argMax' :: [Double] -> Int
    argMax' = fst . maximumBy (comparing snd) . zip [0 ..]

argsortFun :: (MonadError EvalError m) => Value c m
argsortFun =
  VFun $ \case
    VArray xs -> pure $ VArray $ argsort' $ keepNumberValues xs
    _ -> throwError $ RuntimeError "argsort: expecting an array"
  where
    argsort' :: [(Value c m, Double)] -> [Value c m]
    argsort' xs = map (VInt . fst) $ sortOn (snd . snd) $ zip [0 ..] xs

magnitudeFun :: (MonadError EvalError m) => Value c m
magnitudeFun =
  VFun $ \case
    VDouble x -> pure $ VDouble $ abs x
    VInt x -> pure $ VInt $ abs x
    VArray xs -> pure $ VDouble $ sqrt $ sum $ map (\x -> x ** 2) $ map snd (keepNumberValues xs)
    _ -> throwError $ RuntimeError "magnitude: expecting a number"

normFun :: (MonadError EvalError m) => Value c m
normFun = magnitudeFun

appendText :: Text -> Text -> Text
appendText = Text.append

textLength :: Text -> Int64
textLength = fromIntegral . Text.length

stripText :: Text -> Text
stripText = Text.strip

textSplitAt :: (MonadError EvalError m) => Value c m
textSplitAt =
  VFun $ \case
    VInt n -> pure $
      VFun $ \case
        VText txt ->
          let (a, b) = Text.splitAt (fromIntegral n) txt
           in pure $ VTuple [VText a, VText b]
        _ -> throwError $ RuntimeError "splitAt: expecting text for the second argument"
    _ -> throwError $ RuntimeError "splitAt: expecting an int for the first argument"
