module Inferno.Module.Builtin (builtinModule, emptyHash, oneHash, enumBoolHash) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Inferno.Types.Module (BuiltinEnumHash (..), BuiltinFunHash (..), BuiltinModuleHash (..), Module (..), PinnedModule)
import Inferno.Types.Syntax (Expr (..), ExtIdent (..), ImplExpl (..), Scoped (..))
import Inferno.Types.Type
  ( ImplType (..),
    InfernoType (..),
    Namespace (..),
    TCScheme (..),
    TV (..),
    TypeClass (TypeClass),
    TypeMetadata (..),
    typeBool,
    typeDouble,
    typeInt,
    (.->),
  )
import Inferno.Types.VersionControl (VCObjectHash, vcHash)

-- The builtin module is a dummy module only used for typechecking purposes
-- it contains the bool enum, which must exist because the `if...then..else..` that's built into the
-- basic AST type expects a boolean value in its first argument.
-- The module also includes some meta information for the `Some/None` constructors of the `option` type,
-- which is again built into the language.
-- NOTE: the code and the hashes use the old `one/empty` terms for legacy reasons
builtinModule :: PinnedModule ()
builtinModule =
  Module
    { moduleName = "Builtin"
    , moduleOpsTable = mempty
    , moduleObjects = (name2Hash, hash2ty, ())
    , moduleTypeClasses =
        Set.fromList
          [ TypeClass "numeric" [typeInt]
          , TypeClass "numeric" [typeDouble]
          ]
    }
  where
    name2Hash =
      Map.fromList
        [ (ModuleNamespace "Builtin", vcHash $ BuiltinModuleHash "Builtin")
        , (TypeNamespace "bool", enumBoolHash)
        , (EnumNamespace "true", enumBoolHash)
        , (EnumNamespace "false", enumBoolHash)
        , (FunNamespace "None", emptyHash)
        , (FunNamespace "Some", oneHash)
        ]
    hash2ty =
      Map.fromList
        [
          ( enumBoolHash
          , TypeMetadata
              { identExpr = Var () () LocalScope (Expl $ ExtIdent $ Right "_")
              , ty = ForallTC [] Set.empty $ ImplType Map.empty typeBool
              , docs = Just "Boolean type"
              }
          )
        ,
          ( emptyHash
          , TypeMetadata
              { identExpr = Empty ()
              , ty = emptyTy
              , docs = optionMetaText
              }
          )
        ,
          ( oneHash
          , TypeMetadata
              { identExpr = Var () () LocalScope $ Expl $ ExtIdent $ Right "Some"
              , ty = oneTy
              , docs = optionMetaText
              }
          )
        ]

    optionMetaText :: Maybe Text
    optionMetaText =
      Just $
        "Optional type, representing a value which may be undefined.\n"
          <> "`None` indicates no value is present and `Some v` holds a value `v`\n   "
          <> "To test whether an optional `o` holds some value, use `match ... with` and pattern match on `o`:\n"
          <> "~~~\nmatch o with {\n  | Some v -> // use v here\n  | None -> // handle the case where o is None\n}\n~~~"

emptyTy, oneTy, boolTy :: TCScheme
emptyTy = ForallTC [TV 0] Set.empty $ ImplType Map.empty $ TOptional (TVar $ TV 0)
oneTy = ForallTC [TV 0] Set.empty $ ImplType Map.empty $ TVar (TV 0) .-> TOptional (TVar $ TV 0)
boolTy = ForallTC [] Set.empty $ ImplType Map.empty typeBool

emptyHash, oneHash, enumBoolHash :: VCObjectHash
emptyHash = builtinFunHash "empty" emptyTy
oneHash = builtinFunHash "one" oneTy
enumBoolHash = vcHash $ BuiltinEnumHash boolTy

builtinFunHash :: Text -> TCScheme -> VCObjectHash
builtinFunHash n ty = vcHash $ BuiltinFunHash (Var () () LocalScope $ Expl $ ExtIdent $ Right n, ty)
