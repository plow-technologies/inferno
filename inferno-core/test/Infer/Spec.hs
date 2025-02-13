{-# LANGUAGE NamedFieldPuns #-}

module Infer.Spec where

import Control.Monad.Except (runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (evalStateT)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NEList
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (unpack)
import Debug.Trace (trace)
import Inferno.Core (InfernoError (..), Interpreter (..), mkInferno)
import Inferno.Infer (inferTypeReps, unifyRecords)
import Inferno.Infer.Exhaustiveness
  ( Pattern (W),
    cEmpty,
    cEnum,
    cInf,
    cOne,
    cTuple,
    checkUsefullness,
    exhaustive,
  )
import Inferno.Module.Builtin (enumBoolHash)
import qualified Inferno.Module.Prelude as Prelude
import Inferno.Parse (parseTCScheme, parseType)
import Inferno.Parse.Error (prettyError)
import Inferno.Types.Syntax (ExtIdent (..), Ident (..), RestOfRecord (..), typeText)
import Inferno.Types.Type (ImplType (..), InfernoType (..), TCScheme (..), TV (..), TypeClass (..), typeBool, typeDouble, typeInt, typeWord64)
import Inferno.Types.VersionControl (vcHash)
import Inferno.Utils.Prettyprinter (renderPretty)
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe, shouldNotBe)

inferTests :: Spec
inferTests = describe "infer" $
  do
    let simpleType t = ForallTC [] Set.empty (ImplType Map.empty t)

    let tv i = TVar (TV{unTV = i})
    let makeTCs name params = TypeClass{className = name, params = params}
    let addTC = makeTCs "addition"
    let mulTC = makeTCs "multiplication"
    let negTC = makeTCs "negate"
    let numTC = makeTCs "numeric"
    let ordTC = makeTCs "order"
    let repTC = makeTCs "rep"
    let makeType numTypeVars typeClassList t = ForallTC (map (\i -> TV{unTV = i}) [0 .. numTypeVars]) (Set.fromList typeClassList) (ImplType mempty t)

    inferno <- runIO (mkInferno Prelude.builtinModules [] :: IO (Interpreter IO ()))
    let shouldInferTypeFor str t =
          it ("should infer type of \"" <> unpack str <> "\"") $
            case parseAndInfer inferno str of
              Left err -> expectationFailure $ show err
              Right (_ast, t', _typMap, _comments) -> t' `shouldBe` t

    let shouldFailToInferTypeFor str =
          it ("should fail to infer type of \"" <> unpack str <> "\"") $
            case parseAndInfer inferno str of
              Left (ParseError err) -> expectationFailure $ prettyError $ fst $ NEList.head err
              Left (PinError _err) -> pure ()
              Left (InferenceError _err) -> pure ()
              Right _ -> expectationFailure "Should fail to infer a type"

    shouldInferTypeFor "3" $
      makeType 0 [numTC [tv 0], repTC [tv 0]] (TVar $ TV{unTV = 0})
    shouldInferTypeFor "-3" $
      makeType 0 [negTC [tv 0], numTC [tv 0], repTC [tv 0]] (TVar $ TV{unTV = 0})
    shouldInferTypeFor "3+4" $
      makeType
        2
        [addTC [tv 1, tv 2, tv 0], numTC [tv 1], numTC [tv 2], repTC [tv 1, tv 2]]
        (TVar $ TV{unTV = 0})
    shouldInferTypeFor "3.0" $ simpleType typeDouble
    shouldInferTypeFor "-3.14" $ simpleType typeDouble
    shouldInferTypeFor "3.0-2" $ simpleType typeDouble
    shouldInferTypeFor "0x3abc" $ simpleType typeWord64
    shouldInferTypeFor "#true" $ simpleType typeBool
    shouldInferTypeFor "Builtin.#true" $ simpleType typeBool
    shouldInferTypeFor "#true || #false" $ simpleType typeBool
    shouldFailToInferTypeFor "x"
    shouldInferTypeFor "fun x -> x || #false" $ simpleType (TArr typeBool typeBool)
    shouldInferTypeFor "fun x -> x * 2" $
      makeType
        2
        [mulTC [tv 0, tv 2, tv 1], numTC [tv 2], repTC [tv 2]]
        (TArr (TVar (TV{unTV = 0})) (TVar (TV{unTV = 1})))
    shouldInferTypeFor "fun x -> x * 2.0" $
      makeType
        0
        [mulTC [tv 0, typeDouble, typeDouble]]
        (TArr (TVar (TV{unTV = 0})) typeDouble)
    shouldInferTypeFor "(fun x -> x * 2) 3.0" $ simpleType typeDouble
    shouldInferTypeFor "(fun x -> x < 2)" $
      makeType
        0
        [numTC [tv 0], ordTC [tv 0], repTC [tv 0]]
        (TArr (TVar (TV{unTV = 0})) typeBool)
    shouldInferTypeFor "fun x -> x" $
      ForallTC [TV{unTV = 0}] Set.empty (ImplType Map.empty (TArr (TVar (TV{unTV = 0})) (TVar (TV{unTV = 0}))))
    shouldInferTypeFor "?x + 2" $
      ForallTC
        [TV{unTV = 0}, TV{unTV = 1}, TV{unTV = 2}]
        (Set.fromList [addTC [tv 1, tv 2, tv 0], numTC [tv 2], repTC [tv 2]])
        (ImplType (Map.fromList [(ExtIdent $ Right "x", TVar (TV{unTV = 1}))]) (TVar (TV{unTV = 0})))
    shouldInferTypeFor "?x == 2" $
      ForallTC
        [TV{unTV = 0}]
        (Set.fromList [numTC [tv 0], repTC [tv 0]])
        (ImplType (Map.fromList [(ExtIdent $ Right "x", TVar (TV{unTV = 0}))]) typeBool)
    shouldInferTypeFor "let ?x = 3.14 in ?x + 2" $ simpleType typeDouble
    shouldInferTypeFor "let x = 3.14 in x + 2" $ simpleType typeDouble
    shouldInferTypeFor "if #true then Some 2 else None" $
      makeType
        0
        [numTC [tv 0], repTC [tv 0]]
        (TOptional (TVar (TV{unTV = 0})))
    shouldInferTypeFor "2 > 3.0" $ simpleType typeBool
    shouldInferTypeFor "2 == 3.0" $ simpleType typeBool
    -- equality is defined for all types, however comparing function types will always yield #false
    shouldInferTypeFor "(fun x -> x) == (fun x -> x)" $ simpleType typeBool
    shouldFailToInferTypeFor "if 2 then () else ()"
    shouldFailToInferTypeFor "if #true then () else None"
    shouldInferTypeFor "match #true with { | #true -> #false | _ -> #true}" $ simpleType typeBool
    -- inference fails due to exhaustiveness error, even though this program is runtime safe...
    -- however it is also silly and probably not worth trying to "fix"
    shouldFailToInferTypeFor "match #true with { | #true -> #false}"
    shouldFailToInferTypeFor "fun x -> match x with { | #true -> 1 | #false -> 2 | _ -> 3}"
    -- this should fail because it parses '-' as infix
    shouldFailToInferTypeFor "round -1425"
    shouldInferTypeFor "round (-1425)" $ simpleType typeInt

    -- Records:
    shouldInferTypeFor "{}" $ simpleType $ TRecord Map.empty RowAbsent
    shouldInferTypeFor "{name = \"Zaphod\"; age = 391.4}" $
      simpleType $
        TRecord (Map.fromList [(Ident "name", typeText), (Ident "age", typeDouble)]) RowAbsent
    shouldInferTypeFor "let r = {name = \"Zaphod\"; age = 391.4} in r.age" $ simpleType typeDouble
    shouldInferTypeFor "let r = {name = \"Zaphod\"; age = 391.4} in let f = fun r -> r.age in f r + 1" $ simpleType typeDouble
    shouldFailToInferTypeFor "let r = {name = \"Zaphod\"; age = 391.4} in r.age + \" is too old\""
    -- Record field access vs Module.variable
    shouldFailToInferTypeFor "rec.foo"
    shouldInferTypeFor "Array.length []" $ simpleType typeInt
    shouldFailToInferTypeFor "let r = {} in r.x"
    shouldFailToInferTypeFor "let r = {y = 3} in r.x"
    shouldInferTypeFor "let r = {y = 3.2; x = 4.3} in r.x" $ simpleType typeDouble
    shouldFailToInferTypeFor "let Array = {} in Array.length"
    shouldInferTypeFor "let Array = {x = 2.2} in Array.x" $ simpleType typeDouble
    shouldFailToInferTypeFor "let module r = Array in r.x"
    shouldInferTypeFor "let module r = Array in r.length []" $ simpleType typeInt
    shouldInferTypeFor "let f = fun r -> r.age in f {age = 21.1; x = 5.4}" $ simpleType typeDouble
    -- Record polymorphism
    shouldFailToInferTypeFor "let f = fun r -> if #true then r else {age = 1.1} in f {age = 2; ht = 3}"
    shouldInferTypeFor "let f = fun r -> truncateTo 2 r.ht + truncateTo 2 r.wt in f" $
      makeType 0 [] (TArr (TRecord (Map.fromList [(Ident{unIdent = "ht"}, typeDouble), (Ident{unIdent = "wt"}, typeDouble)]) (RowVar (TV{unTV = 0}))) typeDouble)
    shouldFailToInferTypeFor "let f = fun r -> if #true then r else {age = 1.1} in fun r -> let x = r.ht + r.age + 1.1 in f r"
    shouldFailToInferTypeFor "let f = fun r -> r.age in let x = f {age = 21.1} in let y = f {age = \"t\"} in 1"
    shouldFailToInferTypeFor "let f = fun r -> truncateTo 2 r.age in f {age = \"t\"}"
    -- Record patterns
    shouldInferTypeFor "let f = fun r -> match r with { | {x = x; y = y} -> x + y } in f {x = 3.3; y = 5.1}" $ simpleType typeDouble
    shouldFailToInferTypeFor "let f = fun r -> match r with { | {x = x; y = [y, z]} -> x + y | {x = x; y = t} -> x } in f {x = 3.3; y = 5.1}"
    shouldInferTypeFor "let f = fun r -> match r with { | {x = x; y = [y, z]} -> x + y | {x = x; y = t} -> x } in f {x = 3.3; y = [1.2]}" $ simpleType typeDouble
    shouldFailToInferTypeFor "let f = fun r -> match r with { | {x = x; y = (y, z)} -> x + y | {x = x; y = t} -> x } in f {x = 3.3; y = 5.1}"
    -- Duplicate fields
    shouldFailToInferTypeFor "{x = 3.3; y = 5.1; x = 4}"
    shouldFailToInferTypeFor "let f = fun r -> match r with { | {x = x; y = y} -> x + y } in f {x = 3.3; y = 5.1; x = 4}"
    shouldFailToInferTypeFor "let f = fun r -> match r with { | {x = x; y = y; x = z} -> x + y } in f {x = 3.3; y = 5.1}"

    -- Type annotations:
    shouldInferTypeFor "let xBoo : double = 1 in truncateTo 2 xBoo" $ simpleType typeDouble
    shouldFailToInferTypeFor "let xBoo : double = 1 in truncateTo xBoo 3.14"
    shouldFailToInferTypeFor "let xBoo : double = \"boo\" in xBoo"
    shouldInferTypeFor "let foo : forall 'a. {requires negate on 'a} => 'a = 3.3 in foo" $ simpleType typeDouble
    shouldFailToInferTypeFor "let t : int -> int = truncate in t 43.3"
    shouldInferTypeFor "let x : int = 3 in let y = truncate x in y" $ simpleType typeInt
    shouldInferTypeFor "let d : option of int = None in d ? 2" $ simpleType typeInt
    shouldInferTypeFor "let d : forall 'a. {requires numeric on 'a} ⇒ array of (option of 'a) = [] in 1.0" $ simpleType typeDouble

    -- Array pattern matching:
    shouldFailToInferTypeFor "match [1.2, 3, 3] with { | [_, (x, y), _] -> 3 | _ -> 9 }"
    shouldFailToInferTypeFor "match [1.2, 3, 3] with { | [x, [y]] -> 2 | _ -> 9 }"
    shouldFailToInferTypeFor "match [1.2, 3, 3] with { | [3.2, \"d\", 3] -> 2 }"
    shouldFailToInferTypeFor "fun a -> match a with { | [x, y, z] -> truncateTo x y | _ -> 3 }"
    shouldFailToInferTypeFor "match [1.2, 3, 3] with { | [1] -> 2 | _ -> 3 }"
    shouldFailToInferTypeFor "match [1, 2] with { | [x, x] -> 2*x | [x] -> 0 | _ -> 2 }"

    -- Non-exhaustive array patterns:
    shouldFailToInferTypeFor "match [1.2, 3, 3] with { | [] -> 0 | [x, y] -> 2 }"
    shouldFailToInferTypeFor "match [1.2, 3, 3] with { | [x] -> 1 | [x, y] -> 2 }"
    shouldFailToInferTypeFor "match [1, 3] with { | [1] -> 2 | [x] -> 4 | [] -> 3 }"

    -- Redundant array patterns:
    shouldFailToInferTypeFor "match [1, 2] with { | [x, z] -> 1 | [x, y] -> 1 | _ -> 2 }"

    describe "exhaustiveness checker" $
      do
        let boolsPattern =
              [ cEnum f_hash "false"
              , cEnum t_hash "true"
              , cEnum f_hash "false"
              ]
        shouldBeExhaustive boolsPattern
        shouldBeRedundant boolsPattern

        let numsPattern =
              [ cInf (2.3 :: Double)
              , cInf (1.2 :: Double)
              , cInf (3.4 :: Double)
              , cInf (4.0 :: Double)
              , W
              ]
        shouldBeExhaustive numsPattern
        shouldBeUseful numsPattern
        shouldBeInexhaustive $ init numsPattern

        let optionalPattern =
              [ cOne W
              , cOne $ cEnum f_hash "false"
              , cEmpty
              ]
        shouldBeExhaustive optionalPattern
        shouldBeRedundant optionalPattern

        let complexPattern =
              [ cTuple [cOne (cInf (3 :: Int)), cEnum t_hash "true", cInf (5.0 :: Double)]
              , cTuple [cOne W, cEnum t_hash "true", cInf (5.0 :: Double)]
              , cTuple [cOne W, cEnum f_hash "false", W]
              , cTuple [cEmpty, cEnum t_hash "true", cInf (5.0 :: Double)]
              , cTuple [cEmpty, cEnum f_hash "false", cInf (5.0 :: Double)]
              ]
        shouldBeInexhaustive complexPattern
        shouldBeUseful complexPattern

    describe "inferTypeReps" $ do
      Interpreter{typeClasses} <- runIO (mkInferno Prelude.builtinModules [] :: IO (Interpreter IO ()))

      let typeRepsShouldBe fnTy inTys outTy reps = do
            let tcs = either error id $ parseTCScheme fnTy
            let parseTy = either (error . show) id . parseType
            it (unwords ["type reps of", unpack fnTy, show inTys, unpack outTy]) $
              case inferTypeReps typeClasses tcs (map parseTy inTys) (parseTy outTy) of
                Left e -> expectationFailure $ show e
                Right reps' -> reps' `shouldBe` reps

      let typeRepsShouldErr fnTy inTys outTy = do
            let tcs = either error id $ parseTCScheme fnTy
            let parseTy = either (error . show) id . parseType
            it (unwords ["type reps of", unpack fnTy, show inTys, unpack outTy]) $
              case inferTypeReps typeClasses tcs (map parseTy inTys) (parseTy outTy) of
                Left _ -> pure ()
                Right reps' -> expectationFailure $ "expected inferTypeReps to fail but got: " <> show reps'

      -- Some basic tests:
      typeRepsShouldBe
        "forall 'a 'b 'c . {requires addition on 'a 'b 'c} ⇒ 'a → 'b → 'c"
        ["int", "double"]
        "double"
        []
      typeRepsShouldBe
        "forall 'a. {requires numeric on 'a, requires rep on 'a} ⇒ series of 'a → 'a"
        ["series of double"]
        "double"
        [typeDouble]

      -- Some tests with records:

      typeRepsShouldBe
        "forall 'a 'b 'c 'd . {requires addition on 'a 'b 'c} ⇒ {x: 'a; 'd} → 'c"
        ["{x: int; y: int}"]
        "double"
        []

      typeRepsShouldErr
        "forall 'a 'b 'c 'd . {requires addition on 'a 'b 'c} ⇒ {x: 'a; 'd} → 'c"
        ["{z: int; y: int}"]
        "double"

      typeRepsShouldErr
        "forall 'a 'b 'c 'd . {requires addition on 'a 'b 'c} ⇒ {x: 'a; 'd} → 'c"
        ["{x: int; y: int}"]
        "text"

    describe "unifyRecords" $ do
      unificationShouldBeOK
        ([], RowAbsent)
        ([], RowAbsent)
        0

      unificationShouldBeOK
        ([("f1", typeDouble)], RowVar $ TV 1)
        ([("f1", typeDouble)], RowVar $ TV 2)
        3

      unificationShouldBeOK
        ([("f1", typeBool), ("f2", typeDouble)], RowAbsent)
        ([("f2", typeDouble), ("f1", typeBool)], RowAbsent)
        0

      unificationShouldBeOK
        ([("f1", typeInt), ("f2", TVar $ TV 0)], RowAbsent)
        ([("f1", TVar $ TV 1), ("f2", typeDouble)], RowAbsent)
        2

      unificationShouldFail
        ([("f2", TVar $ TV 0)], RowAbsent)
        ([("f1", TVar $ TV 1), ("f2", TVar $ TV 2)], RowAbsent)
        3

      unificationShouldBeOK
        ([("f1", typeInt)], RowVar $ TV 0)
        ([("f1", TVar $ TV 1), ("f2", typeDouble)], RowAbsent)
        2

      unificationShouldFail
        ([("f1", typeInt)], RowAbsent)
        ([("f1", TVar $ TV 1), ("f2", typeDouble)], RowVar $ TV 0)
        2

      unificationShouldBeOK
        ([], RowVar $ TV 0)
        ([("f1", typeInt), ("f2", typeDouble)], RowAbsent)
        1

      unificationShouldFail
        ([("f1", typeInt)], RowVar $ TV 0)
        ([("f2", typeText), ("f3", typeDouble)], RowAbsent)
        1

      unificationShouldBeOK
        ([("f1", typeInt)], RowVar $ TV 0)
        ([("f2", typeText)], RowVar $ TV 1)
        2

      unificationShouldFail
        ([("f1", typeInt), ("f2", typeDouble)], RowVar $ TV 0)
        ([("f2", typeText), ("f3", typeDouble)], RowVar $ TV 1)
        2

      unificationShouldBeOK
        ([("f1", typeInt)], RowVar $ TV 0)
        ([("f2", typeText), ("f3", typeDouble)], RowVar $ TV 1)
        2
  where
    t_hash = vcHash ("true" :: Ident, enumBoolHash)
    f_hash = vcHash ("false" :: Ident, enumBoolHash)

    enum_sigs =
      Map.fromList
        [ (t_hash, Set.fromList [(t_hash, "true"), (f_hash, "false")])
        , (f_hash, Set.fromList [(t_hash, "true"), (f_hash, "false")])
        ]

    printPatts ps = intercalate "\n      " $ map show ps
    shouldBeExhaustive patts =
      it ("patterns\n      " <> printPatts patts <> "\n    should be exhaustive") $
        case exhaustive enum_sigs $ map (: []) patts of
          Just _ps -> expectationFailure "These patterns should be exhaustive"
          Nothing -> pure ()
    shouldBeInexhaustive patts =
      it ("patterns\n      " <> printPatts patts <> "\n    should be inexhaustive") $
        case exhaustive enum_sigs $ map (: []) patts of
          Just _ps -> pure ()
          Nothing -> expectationFailure "These patterns should be inexhaustive"
    shouldBeUseful patts =
      it ("patterns\n      " <> printPatts patts <> "\n    should be useful") $
        checkUsefullness enum_sigs (map (: []) patts) `shouldBe` []
    shouldBeRedundant patts =
      it ("patterns\n      " <> printPatts patts <> "\n    should contain redundant clauses") $
        checkUsefullness enum_sigs (map (: []) patts) `shouldNotBe` []

    unificationShouldBeOK (ts1, trv1) (ts2, trv2) varCount = do
      let pr (ts, trv) = renderPretty (TRecord (Map.fromList ts) trv)
      it (unpack $ "unifyRecords " <> pr (ts1, trv1) <> " " <> pr (ts2, trv2)) $ do
        let sortFields = Map.toAscList . Map.fromList
        let x = unifyRecords [] (sortFields ts1, trv1) (sortFields ts2, trv2) [] [] []
        let y = runIdentity $ runExceptT $ flip evalStateT varCount $ runReaderT x mempty
        case y of
          Left errs ->
            trace ("unification returned errors " <> show errs) $ expectationFailure $ show errs
          Right s' ->
            trace ("unification returned " <> show s') $ pure ()

    unificationShouldFail (ts1, trv1) (ts2, trv2) varCount = do
      let pr (ts, trv) = renderPretty (TRecord (Map.fromList ts) trv)
      it (unpack $ "unifyRecords " <> pr (ts1, trv1) <> " " <> pr (ts2, trv2)) $ do
        let sortFields = Map.toAscList . Map.fromList
        let x = unifyRecords [] (sortFields ts1, trv1) (sortFields ts2, trv2) [] [] []
        let y = runIdentity $ runExceptT $ flip evalStateT varCount $ runReaderT x mempty
        case y of
          Left errs ->
            trace ("unification returned errors " <> show errs) $ pure ()
          Right s' ->
            trace ("unification returned " <> show s') $ expectationFailure "This unification should fail"
