{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Inferno.LSP.ParseInfer
  ( parseErrorDiagnostic,
    inferErrorDiagnostic,
    mkDiagnostic,
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Inferno.Infer.Env (closeOverType)
import Inferno.Infer.Error (Location, TypeError)
import qualified Inferno.Infer.Error as Error
import Inferno.Parse.Error (prettyError)
import Inferno.Types.Syntax
  ( ExtIdent (ExtIdent),
    Ident (Ident),
    InfernoType,
    ModuleName (ModuleName),
    Namespace
      ( EnumNamespace,
        FunNamespace,
        ModuleNamespace,
        OpNamespace,
        TypeNamespace
      ),
    Scoped (LocalScope, Scope),
    TypeClass,
    TypeClassShape (TypeClassShape),
  )
import Inferno.Types.VersionControl (VCObjectHash)
import Inferno.Utils.Prettyprinter (renderDoc)
import Language.LSP.Types
  ( Diagnostic (Diagnostic),
    DiagnosticSeverity (DsError, DsWarning),
    Range,
    mkRange,
  )
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    align,
    hsep,
    indent,
    squotes,
    vsep,
    (<+>),
  )
import Text.Megaparsec.Error (ParseError, ShowErrorComponent)
import Text.Megaparsec.Pos (SourcePos)
import qualified Text.Megaparsec.Pos as Pos

-- | Build a 'Diagnostic' from a source span, severity, source tag, and message.
-- The @- 2@ on lines accounts for the 2-line @fun ... ->@ prefix prepended
-- before parsing.
parseErrorDiagnostic :: (ShowErrorComponent e) => (ParseError Text e, SourcePos) -> Diagnostic
parseErrorDiagnostic (err, pos) =
  mkDiagnostic DsError (Just "inferno.parser") (pos, pos) . Text.pack $ prettyError err

inferErrorDiagnostic :: TypeError SourcePos -> [Diagnostic]
inferErrorDiagnostic = \case
  Error.UnificationFail _ t1 t2 loc ->
    [ inferDiag loc . renderDoc $
        vsep
          [ "Could not match the type"
          , prettyClosedIndented t1
          , "with"
          , prettyClosedIndented t2
          ]
    ]
  Error.AnnotationUnificationFail _ t1 t2 loc ->
    [ inferDiag loc . renderDoc $
        vsep
          [ "The type of this expression"
          , prettyClosedIndented t1
          , "does not match with the annotated type"
          , prettyClosedIndented t2
          ]
    ]
  Error.ExpectedFunction _ t1 t2 loc ->
    [ inferDiag loc . renderDoc $
        vsep
          [ "Expected a function here of type"
          , prettyClosedIndented t1
          , "but instead found"
          , prettyClosedIndented t2
          ]
    ]
  Error.InfiniteType tv t loc ->
    [ inferDiag loc . renderDoc $
        "Could not unify"
          <+> pretty tv
          <+> "~"
          <+> (pretty . closeOverType) t
    ]
  Error.UnboundExtIdent modNm v loc ->
    [ inferDiag loc . renderDoc $
        mconcat
          [ "Unbound variable '"
          , scopePrefix modNm
          , pretty v
          , "'"
          ]
    ]
  Error.UnboundNameInNamespace modNm (Right n) loc ->
    [ inferDiag loc . renderDoc $ case n of
        FunNamespace (Ident v) ->
          hsep
            [ "Unbound variable"
            , squotes . pretty $ scopePrefixText modNm <> v
            ]
        OpNamespace (Ident v) ->
          hsep
            [ "Unbound operator"
            , squotes . pretty $ scopePrefixText modNm <> v
            ]
        ModuleNamespace (ModuleName v) ->
          hsep
            [ "Module"
            , squotes . pretty $ scopePrefixText modNm <> v
            , "could not be found."
            ]
        TypeNamespace (Ident v) ->
          hsep
            [ "Type"
            , squotes . pretty $ scopePrefixText modNm <> v
            , "could not be found."
            ]
        EnumNamespace (Ident c) ->
          vsep
            [ mconcat
                [ "Could not find the enum constructor '#"
                , scopePrefix modNm
                , pretty c
                , "'."
                ]
            , "Make sure the enum you are trying to use has been imported"
            ]
    ]
  Error.UnboundNameInNamespace _ (Left h) loc ->
    [ inferDiag loc . renderDoc $
        hsep
          [ "Object with hash"
          , squotes . pretty $ show h
          , "could not be found."
          ]
    ]
  Error.ImplicitVarTypeOverlap _ (ExtIdent ident) t1 t2 loc ->
    [ inferDiag loc . renderDoc $
        vsep
          [ hsep
              [ "The implicit variable"
              , squotes $ either (("var$" <>) . pretty) pretty ident
              , "has multiple types:"
              ]
          , prettyClosedIndented t1
          , "and"
          , prettyClosedIndented t2
          ]
    ]
  Error.VarMultipleOccurrence (Ident x) loc2 loc1 ->
    [inferDiag loc1 msg, inferDiag loc2 msg]
    where
      msg :: Text
      msg =
        renderDoc $
          vsep
            [ hsep
                [ "Duplicate declarations of"
                , squotes $ pretty x
                , "in the pattern match"
                ]
            , hsep
                [ "at line"
                , pretty (Pos.unPos (fst loc1).sourceLine) <> ","
                , "column"
                , pretty $ Pos.unPos (fst loc1).sourceColumn
                ]
            , hsep
                [ "and line"
                , pretty (Pos.unPos (fst loc2).sourceLine) <> ","
                , "column"
                , pretty $ Pos.unPos (fst loc2).sourceColumn
                ]
            ]
  Error.IfConditionMustBeBool _ t loc ->
    [ inferDiag loc . renderDoc $
        vsep
          [ "The type of the if condition was expected to be a bool, instead I found"
          , prettyClosedIndented t
          ]
    ]
  Error.AssertConditionMustBeBool _ t loc ->
    [ inferDiag loc . renderDoc $
        vsep
          [ "The type of the assert condition was expected to be a bool, instead I found"
          , prettyClosedIndented t
          ]
    ]
  Error.IfBranchesMustBeEqType _ t1 t2 loc1 loc2 ->
    [inferDiag loc1 msg, inferDiag loc2 msg]
    where
      msg :: Text
      msg =
        renderDoc $
          vsep
            [ "The type of both branches of the if statement must be the same, however I found two different types:"
            , prettyClosedIndented t1
            , "and"
            , prettyClosedIndented t2
            ]
  Error.CaseBranchesMustBeEqType _ t1 t2 loc1 loc2 ->
    [inferDiag loc1 msg, inferDiag loc2 msg]
    where
      msg :: Text
      msg =
        renderDoc $
          vsep
            [ "The type of all case branches must be the same, but I found two different types:"
            , prettyClosedIndented t1
            , "and"
            , prettyClosedIndented t2
            ]
  Error.PatternUnificationFail tPat tE p loc ->
    [ inferDiag loc $
        renderDoc $
          vsep
            [ "The type of the pattern does not match the case expression"
            , "expected"
            , prettyClosedIndented tE
            , "but instead found"
            , indent2 $
                hsep
                  [ pretty p
                  , ":"
                  , align . pretty $ closeOverType tPat
                  ]
            ]
    ]
  Error.PatternsMustBeEqType _ t1 t2 p1 p2 loc1 loc2 ->
    [inferDiag loc1 msg, inferDiag loc2 msg]
    where
      msg :: Text
      msg =
        renderDoc $
          vsep
            [ "The type of all case patterns must be the same, but I found two different types:"
            , indent2 $
                hsep
                  [ pretty p1
                  , ":"
                  , align . pretty $ closeOverType t1
                  ]
            , "and"
            , indent2 $
                hsep
                  [ pretty p2
                  , ":"
                  , align . pretty $ closeOverType t2
                  ]
            ]
  Error.TypeClassNotFoundError _ tcls loc ->
    [ inferDiag loc . renderDoc $
        vsep
          [ "Could not find any definitions for"
          , indent2Pretty $ TypeClassShape tcls
          ]
    ]
  Error.TypeClassNoPartialMatch tcls loc ->
    [ inferDiag loc . renderDoc $
        vsep
          [ "Could not find any matching definitions for"
          , indent2Pretty $ TypeClassShape tcls
          ]
    ]
  Error.CouldNotFindTypeclassWitness (Set.toList -> tyCls) loc ->
    [ inferDiag loc . renderDoc $
        vsep $
          prefix : fmap indent2Pretty tyCls
    ]
    where
      prefix :: Doc ann
      prefix =
        "Could not find any matching assignment of types which satisfies the type class constraints:"
  Error.NonExhaustivePatternMatch pat loc ->
    [ inferDiag loc . renderDoc $
        vsep
          [ "The patterns in this case expression are non-exhaustive."
          , "For example, the following pattern is missing:"
          , indent2Pretty pat
          ]
    ]
  Error.UselessPattern (Just pat) loc ->
    [ inferDiag loc . renderDoc $
        vsep
          [ "This case is unreachable, since it is subsumed by the previous pattern"
          , indent2Pretty pat
          ]
    ]
  Error.UselessPattern Nothing loc ->
    [ inferDiag
        loc
        "This case is unreachable, since it is subsumed by the previous patterns"
    ]
  Error.ModuleNameTaken (ModuleName m) loc ->
    [ inferDiag loc . renderDoc $
        vsep
          [ "The chosen module name already exists:"
          , indent2Pretty m
          ]
    ]
  Error.ModuleDoesNotExist (ModuleName m) loc ->
    [ inferDiag loc . renderDoc $
        vsep
          [ "The following module does not exist:"
          , indent2Pretty m
          , "make sure you have imported the module"
          ]
    ]
  Error.NameInModuleDoesNotExist (ModuleName m) (Ident i) loc ->
    [ inferDiag loc . renderDoc $
        vsep
          [ "The module"
          , indent2Pretty m
          , "does not contain:"
          , indent2Pretty i
          ]
    ]
  Error.AmbiguousName (ModuleName m) n loc ->
    [ inferDiag loc . renderDoc $
        vsep
          [ "The name"
          , indent2Pretty n
          , "you are trying to import from"
          , indent2Pretty m
          , "already exists in local scope and would be overshadowed. Consider using:"
          , mconcat
              [ indent2Pretty m
              , "."
              , pretty n
              ]
          ]
    ]
  Error.DuplicateRecordField (Ident f) loc ->
    [ inferDiag loc . renderDoc $
        vsep
          [ "Duplicate record field name:"
          , indent2Pretty f
          ]
    ]
  where
    inferDiag :: Location SourcePos -> Text -> Diagnostic
    inferDiag = mkDiagnostic DsError (Just "inferno.infer")

    scopePrefix :: Scoped ModuleName -> Doc ann
    scopePrefix = \case
      LocalScope -> mempty
      Scope (ModuleName nm) -> pretty nm <> "."

    scopePrefixText :: Scoped ModuleName -> Text
    scopePrefixText = \case
      LocalScope -> ""
      Scope (ModuleName nm) -> nm <> "."

    indent2 :: Doc ann -> Doc ann
    indent2 = indent 2

    prettyClosedIndented :: InfernoType -> Doc ann
    prettyClosedIndented = indent2Pretty . closeOverType

    indent2Pretty :: (Pretty a) => a -> Doc ann
    indent2Pretty = indent 2 . pretty

mkDiagnostic :: DiagnosticSeverity -> Maybe Text -> (SourcePos, SourcePos) -> Text -> Diagnostic
mkDiagnostic sev src (s, e) msg = Diagnostic range (Just sev) Nothing src msg Nothing Nothing
  where
    range :: Range
    range =
      mkRange
        (fromIntegral (Pos.unPos s.sourceLine) - 2)
        (fromIntegral (Pos.unPos s.sourceColumn) - 1)
        (fromIntegral (Pos.unPos e.sourceLine) - 2)
        $ fromIntegral (Pos.unPos e.sourceColumn) - 1
