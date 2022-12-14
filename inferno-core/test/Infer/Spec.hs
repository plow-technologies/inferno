{-# LANGUAGE TypeApplications #-}

module Infer.Spec where

import Data.List (intercalate)
import qualified Data.List.NonEmpty as NEList
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (unpack)
import Inferno.Infer (inferExpr)
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
import Inferno.Infer.Pinned (pinExpr)
import Inferno.Module.Builtin (enumBoolHash)
import Inferno.Parse (parseExpr, prettyError)
import Inferno.Types.Syntax (ExtIdent (..), Ident (..))
import Inferno.Types.Type (ImplType (..), InfernoType (..), TCScheme (..), TV (..), TypeClass (..), typeBool, typeDouble, typeInt, typeWord64)
import Inferno.Types.VersionControl (vcHash)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldNotBe)
import Utils (baseOpsTable, builtinModules, builtinModulesOpsTable, builtinModulesPinMap)

inferTests :: Spec
inferTests = describe "infer" $
  do
    let simpleType t = ForallTC [] Set.empty (ImplType Map.empty t)

    let tv i = TVar (TV {unTV = i})
    let makeTCs name params = TypeClass {className = name, params = params}
    let addTC ts = makeTCs "addition" ts
    let mulTC ts = makeTCs "multiplication" ts
    let negTC ts = makeTCs "negate" ts
    let numTC ts = makeTCs "numeric" ts
    let ordTC ts = makeTCs "order" ts
    let repTC ts = makeTCs "rep" ts
    let makeType numTypeVars typeClassList t = ForallTC (map (\i -> TV {unTV = i}) [0 .. numTypeVars]) (Set.fromList typeClassList) (ImplType mempty t)

    shouldInferTypeFor "3" $
      makeType 0 [numTC [tv 0], repTC [tv 0]] (TVar $ TV {unTV = 0})
    shouldInferTypeFor "-3" $
      makeType 0 [negTC [tv 0], numTC [tv 0], repTC [tv 0]] (TVar $ TV {unTV = 0})
    shouldInferTypeFor "3+4" $
      makeType
        2
        [addTC [tv 1, tv 2, tv 0], numTC [tv 1], numTC [tv 2], repTC [tv 1, tv 2]]
        (TVar $ TV {unTV = 0})
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
        (TArr (TVar (TV {unTV = 0})) (TVar (TV {unTV = 1})))
    shouldInferTypeFor "fun x -> x * 2.0" $
      makeType
        0
        [mulTC [tv 0, typeDouble, typeDouble]]
        (TArr (TVar (TV {unTV = 0})) typeDouble)
    shouldInferTypeFor "(fun x -> x * 2) 3.0" $ simpleType typeDouble
    shouldInferTypeFor "(fun x -> x < 2)" $
      makeType
        0
        [numTC [tv 0], ordTC [tv 0], repTC [tv 0]]
        (TArr (TVar (TV {unTV = 0})) typeBool)
    shouldInferTypeFor "fun x -> x" $
      ForallTC [TV {unTV = 0}] Set.empty (ImplType Map.empty (TArr (TVar (TV {unTV = 0})) (TVar (TV {unTV = 0}))))
    shouldInferTypeFor "?x + 2" $
      ForallTC
        [TV {unTV = 0}, TV {unTV = 1}, TV {unTV = 2}]
        (Set.fromList [addTC [tv 1, tv 2, tv 0], numTC [tv 2], repTC [tv 2]])
        (ImplType (Map.fromList [(ExtIdent $ Right "x", TVar (TV {unTV = 1}))]) (TVar (TV {unTV = 0})))
    shouldInferTypeFor "?x == 2" $
      ForallTC
        [TV {unTV = 0}]
        (Set.fromList [numTC [tv 0], repTC [tv 0]])
        (ImplType (Map.fromList [(ExtIdent $ Right "x", TVar (TV {unTV = 0}))]) typeBool)
    shouldInferTypeFor "let ?x = 3.14 in ?x + 2" $ simpleType typeDouble
    shouldInferTypeFor "let x = 3.14 in x + 2" $ simpleType typeDouble
    shouldInferTypeFor "if #true then Some 2 else None" $
      makeType
        0
        [numTC [tv 0], repTC [tv 0]]
        (TOptional (TVar (TV {unTV = 0})))
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

    describe "exhaustiveness checker" $
      do
        let boolsPattern =
              [ cEnum f_hash "false",
                cEnum t_hash "true",
                cEnum f_hash "false"
              ]
        shouldBeExhaustive boolsPattern
        shouldBeRedundant boolsPattern

        let numsPattern =
              [ cInf (2.3 :: Double),
                cInf (1.2 :: Double),
                cInf (3.4 :: Double),
                cInf (4.0 :: Double),
                W
              ]
        shouldBeExhaustive numsPattern
        shouldBeUseful numsPattern
        shouldBeInexhaustive $ init numsPattern

        let optionalPattern =
              [ cOne W,
                cOne $ cEnum f_hash "false",
                cEmpty
              ]
        shouldBeExhaustive optionalPattern
        shouldBeRedundant optionalPattern

        let complexPattern =
              [ cTuple [cOne (cInf (3 :: Int)), cEnum t_hash "true", cInf (5.0 :: Double)],
                cTuple [cOne W, cEnum t_hash "true", cInf (5.0 :: Double)],
                cTuple [cOne W, cEnum f_hash "false", W],
                cTuple [cEmpty, cEnum t_hash "true", cInf (5.0 :: Double)],
                cTuple [cEmpty, cEnum f_hash "false", cInf (5.0 :: Double)]
              ]
        shouldBeInexhaustive complexPattern
        shouldBeUseful complexPattern
  where
    t_hash = vcHash ("true" :: Ident, enumBoolHash)
    f_hash = vcHash ("false" :: Ident, enumBoolHash)

    shouldInferTypeFor str t =
      it ("should infer type of \"" <> unpack str <> "\"") $
        case parseExpr baseOpsTable builtinModulesOpsTable str of
          Left err -> expectationFailure $ "Failed parsing with: " <> (prettyError $ fst $ NEList.head err)
          Right (ast, _) ->
            case pinExpr builtinModulesPinMap ast of
              Left err -> expectationFailure $ "Failed inference with: " <> show err
              Right pinnedAST ->
                case inferExpr builtinModules pinnedAST of
                  Left err -> expectationFailure $ "Failed inference with: " <> show err
                  Right (_expr, t', _tyMap) -> t' `shouldBe` t

    shouldFailToInferTypeFor str =
      it ("should fail to infer type of \"" <> unpack str <> "\"") $
        case parseExpr baseOpsTable builtinModulesOpsTable str of
          Left err -> expectationFailure $ "Failed parsing with: " <> (prettyError $ fst $ NEList.head err)
          Right (ast, _) ->
            case pinExpr builtinModulesPinMap ast of
              Left _err -> pure ()
              Right pinnedAST ->
                case inferExpr builtinModules pinnedAST of
                  Left _err -> pure ()
                  Right _ -> expectationFailure $ "Should fail to infer a type"

    enum_sigs =
      Map.fromList
        [ (t_hash, Set.fromList [(t_hash, "true"), (f_hash, "false")]),
          (f_hash, Set.fromList [(t_hash, "true"), (f_hash, "false")])
        ]

    printPatts ps = intercalate "\n      " $ map show ps
    shouldBeExhaustive patts =
      it ("patterns\n      " <> printPatts patts <> "\n    should be exhaustive") $
        case exhaustive enum_sigs $ map (: []) patts of
          Just _ps -> expectationFailure $ "These patterns should be exhaustive"
          Nothing -> pure ()
    shouldBeInexhaustive patts =
      it ("patterns\n      " <> printPatts patts <> "\n    should be inexhaustive") $
        case exhaustive enum_sigs $ map (: []) patts of
          Just _ps -> pure ()
          Nothing -> expectationFailure $ "These patterns should be inexhaustive"
    shouldBeUseful patts =
      it ("patterns\n      " <> printPatts patts <> "\n    should be useful") $
        checkUsefullness enum_sigs (map (: []) patts) `shouldBe` []
    shouldBeRedundant patts =
      it ("patterns\n      " <> printPatts patts <> "\n    should contain redundant clauses") $
        checkUsefullness enum_sigs (map (: []) patts) `shouldNotBe` []
