{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Inferno.Module.Prelude where

import Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Inferno.Eval (TermEnv)
import qualified Inferno.Infer.Pinned as Pinned
import Inferno.Module (Module (..), PinnedModule, combineTermEnvs, pinnedModuleHashToTy, pinnedModuleNameToHash)
import Inferno.Module.Builtin (builtinModule)
import Inferno.Module.Cast (Kind0 (toType), ToValue (toValue))
import Inferno.Module.Prelude.Defs
  ( absFun,
    andFun,
    appendText,
    arcCosFun,
    arcSinFun,
    arcTanFun,
    argmaxFun,
    argminFun,
    argsortFun,
    arrayIndexFun,
    arrayIndexOptFun,
    averageFun,
    ceilingFun,
    clearBitFun,
    complementBitFun,
    complementFun,
    consFun,
    cosFun,
    coshFun,
    dayFun,
    daysBeforeFun,
    daysFun,
    divFun,
    doubleToInt,
    dropWhileFun,
    enumFromToInt64,
    eqFun,
    expFun,
    floorFun,
    foldlFun,
    foldrFun,
    formatTime,
    fromBCDFun,
    fromWordFun,
    geqFun,
    gtFun,
    hourFun,
    hoursBeforeFun,
    hoursFun,
    idFun,
    intToDouble,
    keepSomesFun,
    lengthFun,
    leqFun,
    limitFun,
    lnFun,
    logBaseFun,
    logFun,
    ltFun,
    magnitudeFun,
    maxFun,
    maximumFun,
    medianFun,
    minFun,
    minimumFun,
    minutesBeforeFun,
    minutesFun,
    modFun,
    monthFun,
    monthsBeforeFun,
    mulFun,
    negateFun,
    neqFun,
    normFun,
    orFun,
    piFun,
    powFun,
    randomFun,
    recipFun,
    reverseFun,
    roundFun,
    roundToFun,
    secondsBeforeFun,
    secondsFun,
    setBitFun,
    shiftFun,
    sinFun,
    singletonFun,
    sinhFun,
    sqrtFun,
    stripText,
    subFun,
    sumFun,
    tanFun,
    tanhFun,
    testBitFun,
    textLength,
    textSplitAt,
    timeIntervalFun,
    timeToInt,
    toBCDFun,
    toWord16Fun,
    toWord32Fun,
    toWord64Fun,
    truncateFun,
    truncateToFun,
    unconsFun,
    weeksBeforeFun,
    weeksFun,
    xorFun,
    yearFun,
    yearsBeforeFun,
    zeroFun,
    zipFun,
  )
import Inferno.Parse (OpsTable)
import Inferno.Types.Syntax (ModuleName, Scoped (..))
import Inferno.Types.Type (Namespace, TCScheme, TypeMetadata)
import Inferno.Types.Value (ImplEnvM)
import Inferno.Types.VersionControl (Pinned (..), VCObjectHash)
import Inferno.Utils.QQ.Module (infernoModules)
import Prettyprinter (Pretty)

type ModuleMap m c = Map.Map ModuleName (PinnedModule (TermEnv VCObjectHash c (ImplEnvM m c) ()))

baseOpsTable :: forall m c. (MonadThrow m, Pretty c, Eq c) => ModuleMap m c -> OpsTable
baseOpsTable moduleMap =
  let Module {moduleOpsTable = ops, moduleName = modNm} = moduleMap Map.! "Base"
   in IntMap.unionWith (<>) ops (IntMap.map (\xs -> [(fix, Scope modNm, op) | (fix, _, op) <- xs]) ops)

builtinModulesOpsTable :: forall m c. (MonadThrow m, Pretty c, Eq c) => ModuleMap m c -> Map.Map ModuleName OpsTable
builtinModulesOpsTable = Map.map (\Module {moduleOpsTable} -> moduleOpsTable)

builtinModulesPinMap :: forall m c. (MonadThrow m, Pretty c, Eq c) => ModuleMap m c -> Map.Map (Scoped ModuleName) (Map.Map Namespace (Pinned VCObjectHash))
builtinModulesPinMap moduleMap =
  Pinned.openModule "Base" $
    Pinned.insertBuiltinModule $
      Map.foldrWithKey Pinned.insertHardcodedModule mempty $
        Map.map (Map.map Builtin . pinnedModuleNameToHash) moduleMap

builtinModulesTerms :: forall m c. (MonadThrow m, Pretty c, Eq c) => ModuleMap m c -> TermEnv VCObjectHash c (ImplEnvM m c) ()
builtinModulesTerms = combineTermEnvs

preludeNameToTypeMap :: forall m c. (MonadThrow m, Pretty c, Eq c) => ModuleMap m c -> Map.Map (Maybe ModuleName, Namespace) (TypeMetadata TCScheme)
preludeNameToTypeMap moduleMap =
  let unqualifiedN2h = pinnedModuleNameToHash $ moduleMap Map.! "Base"
      n2h =
        Map.unions $
          Map.mapKeys (Nothing,) (pinnedModuleNameToHash builtinModule)
            : Map.mapKeys (Nothing,) unqualifiedN2h
            : [Map.mapKeys (Just nm,) (pinnedModuleNameToHash m `Map.difference` unqualifiedN2h) | (nm, m) <- Map.toList moduleMap]
      h2ty = Map.unions $ pinnedModuleHashToTy builtinModule : [pinnedModuleHashToTy m | m <- Map.elems moduleMap]
   in Map.mapMaybe (`Map.lookup` h2ty) n2h

-- In the definitions below, ###!x### is an anti-quotation to a haskell variable `x` of type `Monad m => (Value m)`
-- This sort of Value is necessary for polymorphic functions such as `map` or `id`
-- The inferno type of this function must be explicitly specified, otherwise a runtime error will occur when typechecking

-- ###x### is an anti-quotation for `x` for which there is a `ToValue` instance.
-- In most cases, the type of a monomorphic `x` can be automatically derived via the `Kind0` typeclass,
-- however, you must write the type explicitly when converting ad-hoc polymorphic functions such as (+)
-- as these require an accompanying definition of a typeclass, via the syntax:
-- `define typeclass_name on t1 ... tn;`.

builtinModules :: (MonadIO m, MonadThrow m, MonadCatch m, Pretty c, Eq c) => ModuleMap m c
builtinModules =
  [infernoModules|

module Number

  @doc The identity function;
  id : forall 'a. 'a -> 'a := ###!idFun###;

  infixl 12 !!;
  infixl 12 !?;

  infixr 11 **;

  infixl 10 *;
  infixl 10 /;
  infixl 10 %;

  infixl 9 +;
  infixl 9 -;

  prefix 19 -;

  define addition on int int int;
  define addition on int double double;
  define addition on double int double;
  define addition on double double double;
  define addition on time timeDiff time;
  define addition on timeDiff time time;
  define addition on timeDiff timeDiff timeDiff;
  define addition on word16 word16 word16;
  define addition on word16 word32 word32;
  define addition on word32 word16 word32;
  define addition on word32 word32 word32;
  define addition on word16 word64 word64;
  define addition on word64 word16 word64;
  define addition on word32 word64 word64;
  define addition on word64 word32 word64;
  define addition on word64 word64 word64;


  @doc Addition on `int`, `double`, `word16/32/64`, `time` and `timeDiff`;
  (+) : forall 'a 'b 'c. {requires addition on 'a 'b 'c} => 'a -> 'b -> 'c := ###sumFun###;

  define subtraction on int int int;
  define subtraction on int double double;
  define subtraction on double int double;
  define subtraction on double double double;
  define subtraction on time timeDiff time;
  define subtraction on timeDiff timeDiff timeDiff;
  define subtraction on word16 word16 word16;
  define subtraction on word32 word32 word32;
  define subtraction on word64 word64 word64;

  @doc Subtraction on `int`, `double`, `word16/32/64`, `time` and `timeDiff`;
  (-) : forall 'a 'b 'c. {requires subtraction on 'a 'b 'c} => 'a -> 'b -> 'c := ###subFun###;

  define division on int int int;
  define division on int double double;
  define division on double int double;
  define division on double double double;

  @doc Division on `int`, `double`;
  (/) : forall 'a 'b 'c. {requires division on 'a 'b 'c} => 'a -> 'b -> 'c := ###divFun###;

  define multiplication on int int int;
  define multiplication on int double double;
  define multiplication on double int double;
  define multiplication on double double double;
  define multiplication on int timeDiff timeDiff;
  define multiplication on timeDiff int timeDiff;

  @doc Multiplication on `int`, `double`;
  (*) : forall 'a 'b 'c. {requires multiplication on 'a 'b 'c} => 'a -> 'b -> 'c := ###mulFun###;

  @doc Reciprocal fraction;
  recip : double -> double := ###recipFun###;

  define power on int;
  define power on double;

  @doc `x ** y` raises `x` to the power of `y`, where the arguments are `int` or `double`;
  (**) : forall 'a. {requires power on 'a} => 'a -> 'a -> 'a := ###powFun###;

  @doc Square root;
  sqrt : double -> double := ###sqrtFun###;

  @doc Exponential function;
  exp : double -> double := ###expFun###;

  @doc Natural logarithm;
  ln : double -> double := ###lnFun###;

  @doc Logarithm with base `10`;
  log : double -> double := ###logFun###;

  @doc Logarithm with base `b`;
  logBase : double -> double -> double := ###logBaseFun###;

  define negate on int;
  define negate on double;
  define negate on timeDiff;

  @doc Negation (unary) on `int`, `double`, or `timeDiff`;
  - : forall 'a. {requires negate on 'a} => 'a -> 'a := ###negateFun###;

  define abs on int;
  define abs on double;
  define abs on timeDiff;

  @doc Absolute value (sometimes written |x|) on `int`, `double`, or `timeDiff`;
  abs : forall 'a. {requires abs on 'a} => 'a -> 'a := ###absFun###;

  @doc Modulus operator. `n % m` is the remainder obtained when `n` is divided by `m`. E.g. `5 % 3 == 2`;
  (%) : int -> int -> int := ###modFun###;

  define roundable on double;
  define roundable on int;

  @doc Floor function (rounds down to nearest integer);
  floor : forall 'a. {requires roundable on 'a} => 'a -> int := ###floorFun###;

  @doc Ceiling function (rounds up to nearest integer);
  ceiling : forall 'a. {requires roundable on 'a} => 'a -> int := ###ceilingFun###;

  @doc `round x` returns the nearest integer to `x`
  (the even integer if `x` is equidistant between two integers);
  round : forall 'a. {requires roundable on 'a} => 'a -> int := ###roundFun###;

  @doc `roundTo d x` rounds `x` to `d` decimal places;
  roundTo : int -> double -> double := ###roundToFun###;

  @doc `truncate x` returns the integer nearest `x` between zero and `x`;
  truncate : forall 'a. {requires roundable on 'a} => 'a -> int := ###truncateFun###;

  @doc `truncateTo d x` truncates `x` to `d` decimal places;
  truncateTo : int -> double -> double := ###truncateToFun###;

  @doc `limit l u x = min l (max x u)` limits `x` to be between `l` and `u`;
  limit : double -> double -> double -> double := ###limitFun###;

  @doc Pi (`3.14159... :: double`), the constant;
  pi : double := ###piFun###;

  @doc Sine (trigonometric function) of a `double`;
  sin : double -> double := ###sinFun###;

  @doc Hyperbolic sine (trigonometric function) of a `double`;
  sinh : double -> double := ###sinhFun###;

  @doc Inverse sine (trigonometric function) of a `double`;
  arcSin : double -> double := ###arcSinFun###;

  @doc Cosine (trigonometric function), on `double`;
  cos : double -> double := ###cosFun###;

  @doc Hyperbolic cosine (trigonometric function) of a `double`;
  cosh : double -> double := ###coshFun###;

  @doc Inverse cosine (trigonometric function) of a `double`;
  arcCos : double -> double := ###arcCosFun###;

  @doc Tan (trigonometric function), on `double`;
  tan : double -> double := ###tanFun###;

  @doc Hyperbolic tangent (trigonometric function) of a `double`;
  tanh : double -> double := ###tanhFun###;

  @doc Inverse tan (trigonometric function) of a `double`;
  arcTan : double -> double := ###arcTanFun###;

  @doc Convert int to double;
  intToDouble : int -> double := ###intToDouble###;

  @doc Convert double to int;
  doubleToInt : double -> int := ###doubleToInt###;

  @doc A (pseudo)random `double` number in the `[0, 1)` interval. To obtain a random number between 20 and 30, use `(10 * random ()) + 20`. The `()` argument is needed so that each time `random` is called a new random value is generated.;
  random : () -> double := ###!randomFun###;

module Option
  @doc `Option.reduce f o d` unwraps an optional value `o` and applies `f` to it, if o contains a `Some` value. Otherwise it returns the default value `d`.
  ~~~inferno
  Option.reduce (fun str -> `${str} there!`) "hi" (Some "hello") == "hello there!"
  Option.reduce (fun str -> `${str} there!`) "hi" None == "hi"
  ~~~;
  reduce : forall 'a 'b. ('a -> 'b) -> 'b -> option of 'a -> 'b :=
    fun f b ma -> match ma with {
      | Some a -> f a
      | None -> b
    };

  @doc `Option.map f ma` applies `f` to the value inside `ma`, namely if `ma` is `Some a`, it will return `Some (f a)`.;
  map : forall 'a 'b. ('a -> 'b) -> option of 'a -> option of 'b :=
    fun f ma -> match ma with {
      | Some a -> Some (f a)
      | None -> None
    };
  singleton : forall 'a. 'a -> option of 'a := fun a -> Some a;

  @doc Given a tuple `(Some x , Some y)`, `Option.mergeTuple` returns `Some (x , y)`. If any of the components are `None` it returns `None`.;
  mergeTuple : forall 'a 'b. (option of 'a, option of 'b) -> option of ('a, 'b) :=
    fun ab -> match ab with {
      | (Some a , Some b) -> Some (a , b)
      | _ -> None
    };

  @doc `Option.join` removes the outer "layer" of a nested option. (By definition, `Option.join == Option.reduce id None`).
  ~~~inferno
  Option.join None == None
  Option.join (Some None) == None
  Option.join (Some (Some a)) == Some a
  ~~~;
  join : forall 'a. option of (option of 'a) -> option of 'a := reduce Number.id None;

module Array

  @doc Array indexing: gets the ith element of an array. Throws a RuntimeError if i is out of bounds.;
  get : forall 'a. array of 'a -> int -> 'a := ###!arrayIndexFun###;

  @doc Safe array indexing: gets the ith element of an array. Returns None if i is out of bounds.;
  getOpt : forall 'a. array of 'a -> int -> option of 'a := ###!arrayIndexOptFun###;

  @doc This function can be used to create an array with a single element:
  ~~~inferno
  singleton 2
  ~~~;
  singleton : forall 'a. 'a -> array of 'a := ###!singletonFun###;

  length : forall 'a. array of 'a -> int := ###!lengthFun###;

  @doc The minimum value in an array, or `None` if empty;
  minimum: forall 'a. {requires order on 'a} => array of 'a -> option of 'a := ###!minimumFun###;

  @doc The maximum value in an array, or `None` if empty;
  maximum: forall 'a. {requires order on 'a} => array of 'a -> option of 'a := ###!maximumFun###;

  @doc The average of the values in an array, or `None` if empty;
  average: forall 'a. {requires numeric on 'a} => array of 'a -> option of double := ###!averageFun###;

  @doc Return the median of the values in an array, or `None` if empty;
  median: forall 'a. {requires numeric on 'a} => array of 'a -> option of double := ###!medianFun###;

  @doc The index of the minimum value in an array, or `None` if empty;
  argmin: forall 'a. {requires order on 'a} => array of 'a -> option of int := ###!argminFun###;

  @doc The index of the maximum value in an array, or `None` if empty;
  argmax: forall 'a. {requires order on 'a} => array of 'a -> option of int := ###!argmaxFun###;

  @doc Returns the indices that would sort an array;
  argsort: forall 'a. {requires order on 'a} => array of 'a -> array of int := ###!argsortFun###;

  @doc Returns the Euclidean norm of an array, or `None` if empty;
  magnitude: forall 'a. {requires numeric on 'a} => array of 'a -> option of double := ###!magnitudeFun###;

  @doc Returns the Euclidean norm of an array, or `None` if empty;
  norm: forall 'a. {requires numeric on 'a} => array of 'a -> option of double := ###!normFun###;

  @doc The `Array.range` function takes two `int` arguments `n` and `m` and produces an array `[n,...,m]`.
  If `m` > `n`, the empty array is returned.;
  range := ###enumFromToInt64###;

  @doc The `Array.map` function takes a function `f` and an array of elements and applies `f` to each one;
  map := fun f xs -> [f x | x <- xs];

  @doc `Array.keepSomes` discards any `None` values in an array and unwaps all `Some`s.
  ~~~inferno
  Array.keepSomes [None, Some "hello", None, Some "world"] = ["hello", "world"]
  ~~~;
  keepSomes : forall 'a. array of (option of 'a) -> array of 'a := ###!keepSomesFun###;

  @doc `reduce` applied to a binary operator, a starting value (typically an identity of the operator, e.g. `0`), and an array, reduces the array using the binary operator, from left to right:
  ~~~inferno
  reduce f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
  ~~~
  Example: `reduce (+) 42 [1, 2, 3, 4] == 52`;
  reduce : forall 'a 'b. ('b -> 'a -> 'b) -> 'b -> array of 'a -> 'b := ###!foldlFun###;

  @doc `reduceRight`, applied to a binary operator, a starting value (typically an identity of the operator), and a array, reduces the array using the binary operator, from right to left:
  ~~~inferno
  reduceRight f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
  ~~~
  Example: `reduceRight (+) 42 [1, 2, 3, 4] == 52`, but `reduceRight (-) 0 [5, 3] == 2` while `reduceRight (-) 0 [3, 5] == -2`;
  reduceRight : forall 'a 'b. ('a -> 'b -> 'b) -> 'b -> array of 'a -> 'b := ###!foldrFun###;

  define zero on int;
  define zero on double;
  define zero on word16;
  define zero on word32;
  define zero on word64;
  define zero on timeDiff;

  zero : forall 'a. {requires rep on 'a, requires zero on 'a} => 'a := ###!zeroFun###;

  @doc `Array.sum` computes the sum of elements in an array. The elements can be of type `int`/`double`/`word`.;
  sum := reduce Number.(+) zero;

  findFirstSome : forall 'a. array of (option of 'a) -> option of 'a :=
    reduceRight
      (fun a rest -> match a with {
        | Some _ -> a
        | None -> rest
      })
      None;
  findLastSome : forall 'a. array of (option of 'a) -> option of 'a :=
    reduce
      (fun rest a -> match a with {
        | Some _ -> a
        | None -> rest
      })
      None;
  findFirstAndLastSome : forall 'a. array of (option of 'a) -> option of ('a, 'a) :=
    let firstLast =
      reduce
        (fun acc a -> match (a , acc) with {
          | (Some _ , (None , _)) -> (a , a)
          | (Some _ , (Some v , _)) -> (Some v , a)
          | (None , _) -> acc
        })
        (None, None)
    in fun arr -> Option.mergeTuple (firstLast arr);

  reverse : forall 'a. array of 'a -> array of 'a := ###!reverseFun###;

  dropWhile : forall 'a. ('a -> bool{#true, #false}) -> array of 'a -> array of 'a := ###!dropWhileFun###;
  // dropWhile : forall 'a. ('a -> int) -> array of 'a -> array of 'a := ###!dropWhileFun###;

module Text

  append : text -> text -> text := ###appendText###;

  length : text -> int := ###textLength###;

  strip : text -> text := ###stripText###;

  splitAt : int -> text -> (text, text) := ###!textSplitAt###;

module Time

  toTime : timeDiff -> time := ###!idFun###;

  seconds : int -> timeDiff := ###secondsFun###;

  minutes : int -> timeDiff := ###minutesFun###;

  hours : int -> timeDiff := ###hoursFun###;

  days : int -> timeDiff := ###daysFun###;

  weeks : int -> timeDiff := ###weeksFun###;

  secondsBefore : time -> int -> time := ###secondsBeforeFun###;

  minutesBefore : time -> int -> time := ###minutesBeforeFun###;

  hoursBefore : time -> int -> time := ###hoursBeforeFun###;

  daysBefore : time -> int -> time := ###daysBeforeFun###;

  weeksBefore : time -> int -> time := ###weeksBeforeFun###;

  @doc Subtracts the given number of months, with days past the last day of the month clipped to the last day. For instance, 1 month before 2005-03-30 is 2005-02-28.;
  monthsBefore : time -> int -> time := ###monthsBeforeFun###;

  @doc Subtracts the given number of years, with days past the last day of the month clipped to the last day. For instance, 2 years before 2006-02-29 is 2004-02-28.;
  yearsBefore : time -> int -> time := ###yearsBeforeFun###;

  @doc Rounds down to the start of the nearest hour;
  hour : time -> time := ###hourFun###;

  @doc Rounds down to the start of the nearest day;
  day : time -> time := ###dayFun###;

  @doc Rounds down to the start of the nearest month;
  month : time -> time := ###monthFun###;

  @doc Rounds down to the start of the nearest year;
  year : time -> time := ###yearFun###;

  intervalEvery : timeDiff -> time -> time -> array of time := ###timeIntervalFun###;

  @doc Convert time to int (returned as number of seconds since January 1, 1970, 00:00, not counting leap seconds);
  timeToInt : time -> int := ###timeToInt###;

  @doc Format time to string;
  formatTime : time -> text -> text := ###formatTime###;

module Word

  infixr 5 &&;
  infixr 4 XOR;
  infixr 3 ||;

  prefix 19 !;

  define bitlike on bool{#true, #false};
  define bitlike on word16;
  define bitlike on word32;
  define bitlike on word64;

  @doc Checks if the bit at the provided offset is set;
  testBit : forall 'a. {requires bitlike on 'a} => 'a -> int -> bool{#true, #false} := ###testBitFun###;

  @doc Sets the bit at the provided offset to `1`;
  setBit : forall 'a. {requires bitlike on 'a} => 'a -> int -> 'a := ###setBitFun###;

  @doc Clears the bit at the provided offset (i.e., sets it to `0`);
  clearBit : forall 'a. {requires bitlike on 'a} => 'a -> int -> 'a := ###clearBitFun###;

  @doc Switches the bit at the provided offset (`0` to `1` or vice versa);
  complementBit : forall 'a. {requires bitlike on 'a} => 'a -> int -> 'a := ###complementBitFun###;

  @doc `shift x i` shifts `x` left by `i` bits if `i` is positive, or right by `-i` bits otherwise.  Right shifts perform sign extension on signed number types (i.e. they fill the top bits with `1` if `x` is negative and with `0` otherwise).
  Examples:
  ~~~inferno
  shift 0x0F0 4 == 0xF00
  shift 0x0F0 (-4) == 0x00F
  shift 0x0F0 (-8) == 0x000
  ~~~;
  shift : forall 'a. {requires bitlike on 'a} => 'a -> int -> 'a := ###shiftFun###;

  @doc Bitwise AND;
  (&&) : forall 'a. {requires bitlike on 'a} => 'a -> 'a -> 'a := ###andFun###;

  @doc Bitwise OR;
  (||) : forall 'a. {requires bitlike on 'a} => 'a -> 'a -> 'a := ###orFun###;

  @doc Bitwise XOR;
  (XOR) : forall 'a. {requires bitlike on 'a} => 'a -> 'a -> 'a := ###xorFun###;

  @doc Complements (switches 0s and 1s) all bits in the argument;
  ! : forall 'a. {requires bitlike on 'a} => 'a -> 'a := ###complementFun###;

  define toWord64 on bool{#true, #false};
  define toWord64 on int;
  define toWord64 on word16;
  define toWord64 on word32;
  define toWord64 on word64;

  @doc Convert an `int` or another word to a `word64`;
  toWord64 : forall 'a. {requires toWord64 on 'a} => 'a -> word64 := ###toWord64Fun###;

  define toWord32 on bool{#true, #false};
  define toWord32 on int;
  define toWord32 on word16;
  define toWord32 on word32;
  define toWord32 on word64;

  @doc Convert an `int` or another word to a `word32`, dropping bits if necessary;
  toWord32 : forall 'a. {requires toWord32 on 'a} => 'a -> word32 := ###toWord32Fun###;

  define toWord16 on bool{#true, #false};
  define toWord16 on int;
  define toWord16 on word16;
  define toWord16 on word32;
  define toWord16 on word64;

  @doc Convert an `int` or another word to a `word16`, dropping bits if necessary;
  toWord16 : forall 'a. {requires toWord16 on 'a} => 'a -> word16 := ###toWord16Fun###;

  define fromWord on bool{#true, #false};
  define fromWord on word16;
  define fromWord on word32;
  define fromWord on word64;

  @doc Convert a word to an `int`;
  fromWord : forall 'a. {requires fromWord on 'a} => 'a -> int := ###fromWordFun###;

  @doc Decode a BCD-encoded word64;
  fromBCD : word64 -> option of word64 := ###fromBCDFun###;

  @doc Encode a BCD-encoded word64;
  toBCD : word64 -> word64 := ###toBCDFun###;

module Base

  infix 7 >=;
  infix 7 >;
  infix 7 <=;
  infix 7 <;
  infix 6 ==;
  infix 6 !=;
  infix 19 ..;

  infixr 5 ?;
  // infixl 5 <|>;

  infixl 12 <<;
  infixl 12 |>;

  @doc Function composition. `(f << g) x == f (g x)`;
  (<<) : forall 'a 'b 'c. ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c := fun f g x -> f (g x);

  @doc The pipe operator. `x |> f |> g == g (f x)`;
  (|>) : forall 'a 'b 'c. 'a -> ('a -> 'b) -> 'b := fun x f -> f x;

  (..) := ###enumFromToInt64###;

  define order on int;
  define order on double;
  define order on time;
  define order on timeDiff;

  @doc Ordering on `int`, `double`, `word16/32/64`, `time` and `timeDiff`;
  (>) : forall 'a. {requires order on 'a} => 'a -> 'a -> bool{#true, #false} := ###gtFun###;

  @doc Ordering on `int`, `double`, `word16/32/64`, `time` and `timeDiff`;
  (>=) : forall 'a. {requires order on 'a} => 'a -> 'a -> bool{#true, #false} := ###geqFun###;

  @doc Ordering on `int`, `double`, `word16/32/64`, `time` and `timeDiff`;
  (<) : forall 'a. {requires order on 'a} => 'a -> 'a -> bool{#true, #false} := ###ltFun###;

  @doc Ordering on `int`, `double`, `word16/32/64`, `time` and `timeDiff`;
  (<=) : forall 'a. {requires order on 'a} => 'a -> 'a -> bool{#true, #false} := ###leqFun###;

  @doc The equality function works on any value of the same type. Always returns `#false` for functions;
  (==) : forall 'a. 'a -> 'a -> bool{#true, #false} := ###!eqFun###;

  @doc The (not) equals function works on any value of the same type. Always returns `#true` for functions;
  (!=) : forall 'a. 'a -> 'a -> bool{#true, #false} := ###!neqFun###;

  @doc Minimum function on `int`, `double`, `word16/32/64`, `time` and `timeDiff`;
  min : forall 'a. {requires order on 'a} => 'a -> 'a -> 'a := ###minFun###;

  @doc Maximum function on `int`, `double`, `word16/32/64`, `time` and `timeDiff`;
  max : forall 'a. {requires order on 'a} => 'a -> 'a -> 'a := ###maxFun###;

  export Number;
  export Word;

  @doc Array indexing: an infix operator to get the ith element of an array. Throws a RuntimeError if i is out of bounds.;
  (!!) : forall 'a. array of 'a -> int -> 'a := Array.get;

  @doc Safe array indexing: an infix operator to get the ith element of an array. Returns None if i is out of bounds.;
  (!?) : forall 'a. array of 'a -> int -> option of 'a := Array.getOpt;

  cons : forall 'a. 'a -> array of 'a -> array of 'a := ###!consFun###;

  // TODO why doesn't this work?
  // (:) : forall 'a. 'a -> array of 'a -> array of 'a := cons;

  uncons : forall 'a. array of 'a -> option of ('a, array of 'a) := ###!unconsFun###;

  @doc The `fromOption` function unwraps an optional value, if given a default value to fall back on in case the value of the optional is `None`.
  ~~~inferno
  fromOption "hi" (Some "hello") == "hello"
  fromOption "hi" None == "hi"
  ~~~;
  fromOption : forall 'a. 'a -> option of 'a -> 'a :=
    fun default ma -> match ma with {
      | Some a -> a
      | None -> default
    };

  @doc The `?` function unwraps an optional value, if given a default value to fall back on in case the value of the optional is `None`.
  ~~~inferno
  (Some "hello") ? "hi" == "hello"
  None ? "hi" == "hi"
  ~~~;
  (?) : forall 'a. option of 'a -> 'a -> 'a :=
    fun ma default -> match ma with {
      | Some a -> a
      | None -> default
    };

  // TODO fix parser and re-introduce this
  // @doc The `<|>` operator implements choice: it returns the left value if not `None`, otherwise the right value.
  // ~~~inferno
  // (Some "hello") <|> Some "hi" == Some "hello"
  // None <|> Some "hi" == Some "hi"
  // ~~~;
  // (<|>) : forall 'a. option of 'a -> option of 'a -> option of 'a :=
  //   fun ma default -> match ma with {
  //     | Some a -> Some a
  //     | None -> default
  //   };

  @doc Gets the first component of a tuple: `fst (x, y) == x`;
  fst : forall 'a 'b. ('a, 'b) -> 'a := fun t -> match t with { (x, y) -> x };

  @doc Gets the second component of a tuple: `snd (x, y) == y`;
  snd : forall 'a 'b. ('a, 'b) -> 'b := fun t -> match t with { (x, y) -> y };

  @doc Zip two arrays into a array of tuples/pairs. If one input array is shorter than the other, excess elements of the longer array are discarded. `zip [1, 2] ['a', 'b'] == [(1,'a'),(2,'b')]`;
  zip : forall 'a 'b. array of 'a -> array of 'b -> array of ('a, 'b) := ###!zipFun###;

|]
