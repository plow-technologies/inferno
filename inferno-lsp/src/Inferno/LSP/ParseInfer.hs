{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Parse\/infer pipeline and diagnostic generation.
--
-- This module is concerned with language-level operations only: parsing,
-- type inference, comment insertion, and unused-variable detection. It does
-- NOT handle:
--
--   * Input type validation (@validateInput@); this is domain policy and
--     belongs in the Server layer.
--   * 'Inferno.LSP.Hover.HoverIndex' construction; the Server builds it from
--     'InferSuccess.typeMap'.
--   * Backwards-compat @ParsedResult@ translation (the @(Expr, TCScheme,
--     [Diagnostic], [(Range, MarkupContent)])@ tuple expected by @afterParse@
--     callbacks); the Server is responsible for assembling it from
--     'InferSuccess' and the hover rendering functions in "Inferno.LSP.Hover".
module Inferno.LSP.ParseInfer
  ( InferSuccess
      ( InferSuccess,
        ast,
        scheme,
        typeMap,
        classes,
        warnings
      ),
    parseAndInferWithTimeout,
    parseAndInferDiagnostics,
    parseErrorDiagnostic,
    inferErrorDiagnostic,
    mkDiagnostic,
  ) where

import Control.Exception (evaluate)
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Foldable (foldl') -- NOTE: Do NOT remove, needed for GHC version compat
import Data.Functor (($>))
import Data.List.Extra (nubOrd)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Inferno.Core (Interpreter)
import qualified Inferno.Core as Core
import Inferno.Infer.Env (closeOverType)
import Inferno.Infer.Error (Location, TypeError)
import qualified Inferno.Infer.Error as Error
import Inferno.Parse.Commented (insertCommentsIntoExpr)
import Inferno.Parse.Error (prettyError)
import Inferno.Types.Syntax
  ( Expr (Lam),
    ExtIdent (ExtIdent),
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
    TCScheme,
    TypeClass,
    TypeClassShape (TypeClassShape),
    TypeMetadata,
    collectArrs,
    unusedVars,
  )
import qualified Inferno.Types.Syntax
import Inferno.Types.VersionControl (Pinned, VCObjectHash)
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
import UnliftIO.Timeout (timeout)
import qualified Text.Megaparsec.Pos as Pos

-- | The result of a successful parse\/infer cycle.
data InferSuccess = InferSuccess
  { ast :: !(Expr (Pinned VCObjectHash) ())
  , scheme :: !TCScheme
  , typeMap :: !(Map (SourcePos, SourcePos) (TypeMetadata TCScheme))
  , classes :: !(Set TypeClass)
  , warnings :: ![Diagnostic]
  }

-- | Parse and infer a script with a 120s timeout and input type validation.
-- Intended for use outside the LSP session (e.g. script upload endpoints).
parseAndInferWithTimeout ::
  (Pretty c, Eq c) =>
  Interpreter IO c ->
  [Maybe Ident] ->
  Text ->
  (InfernoType -> Either Text ()) ->
  IO (Either [Diagnostic] (Expr (Pinned VCObjectHash) (), TCScheme))
parseAndInferWithTimeout interp idents txt validate =
  fmap (fromMaybe (Left [timeoutDiag]) . fmap validated) . timeout 120_000_000 $
    case parseAndInferDiagnostics interp idents txt of
      l@(Left _) -> evaluate l
      r@(Right s) -> evaluate s *> evaluate r
  where
    validated :: Either [Diagnostic] InferSuccess -> Either [Diagnostic] (Expr (Pinned VCObjectHash) (), TCScheme)
    validated = \case
      Left ds -> Left ds
      Right s ->
        first (pure . mkDiagnostic DsError (Just "inferno.validate") (startPos, startPos))
          $ validate s.scheme.impl.body
          $> (s.ast, s.scheme)

    startPos :: SourcePos
    startPos = Pos.initialPos mempty

    timeoutDiag :: Diagnostic
    timeoutDiag =
      mkDiagnostic DsError (Just "inferno.lsp") (startPos, startPos) "Inferno timed out in 120s"

-- | Run the parse\/infer pipeline on script text, producing either error
-- diagnostics or a successful result. Handles the @fun args ->@ prefix
-- construction, comment insertion, and unused-variable warnings.
parseAndInferDiagnostics ::
  (Pretty c, Eq c) =>
  Interpreter IO c ->
  [Maybe Ident] ->
  Text ->
  Either [Diagnostic] InferSuccess
parseAndInferDiagnostics interp idents txt =
  case interp.parseAndInfer input of
    Left (Core.ParseError errs) ->
      Left . fmap parseErrorDiagnostic $ NonEmpty.toList errs
    Left (Core.PinError errs) ->
      Left . foldMap inferErrorDiagnostic $ nubOrd errs
    Left (Core.InferenceError errs) ->
      Left . foldMap inferErrorDiagnostic $ nubOrd errs
    Right (pinnedAst, scheme, typeMap, comments)
      | length (collectArrs scheme.impl.body) > length idents + 1 ->
          Left [scriptIsFunctionErr]
      | otherwise ->
          Right
            InferSuccess
              { ast =
                  putBackLams lams . void $
                    insertCommentsIntoExpr comments body
              , classes = scheme.classes
              , warnings = unusedVarWarnings body
              , scheme
              , typeMap
              }
      where
        lams :: [NonEmpty (Maybe ExtIdent)]
        body :: Expr (Pinned VCObjectHash) SourcePos
        (lams, body) = extractLams mempty pinnedAst
  where
    input :: Text
    input = case idents of
      [] -> "\n" <> txt
      ids ->
        mconcat
          [ "fun "
          , Text.intercalate " " $ fmap (maybe "_" (.unIdent)) ids
          , " -> \n"
          , txt
          ]

    extractLams ::
      [NonEmpty (Maybe ExtIdent)] ->
      Expr (Pinned VCObjectHash) SourcePos ->
      ([NonEmpty (Maybe ExtIdent)], Expr (Pinned VCObjectHash) SourcePos)
    extractLams acc = \case
      Lam _ xs _ e -> extractLams (fmap snd xs : acc) e
      e -> (acc, e)

    putBackLams ::
      [NonEmpty (Maybe ExtIdent)] ->
      Expr (Pinned VCObjectHash) () ->
      Expr (Pinned VCObjectHash) ()
    putBackLams acc e = foldl' wrapLam e acc
      where
        wrapLam ::
          Expr (Pinned VCObjectHash) () ->
          NonEmpty (Maybe ExtIdent) ->
          Expr (Pinned VCObjectHash) ()
        wrapLam l xs = Lam () (fmap ((),) xs) () l

    scriptIsFunctionErr :: Diagnostic
    scriptIsFunctionErr =
      mkDiagnostic
        DsError
        (Just "inferno.infer")
        (startPos, startPos)
        "This script evaluates to a function. Did you mean to add input parameters instead?"

    startPos :: SourcePos
    startPos = Pos.initialPos mempty

    unusedVarWarnings :: Expr (Pinned VCObjectHash) SourcePos -> [Diagnostic]
    unusedVarWarnings =
      fmap mkWarn . Set.toList . unusedVars
      where
        mkWarn :: (Text, SourcePos, SourcePos) -> Diagnostic
        mkWarn (x, s, e) =
          mkDiagnostic DsWarning (Just "inferno.lsp") (s, e) $
            "Unused variable: " <> x

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

-- | Build a 'Diagnostic' from a source span, severity, source tag, and message.
-- The @- 2@ on lines accounts for the 2-line @fun ... ->@ prefix prepended
-- before parsing.
mkDiagnostic ::
  DiagnosticSeverity -> Maybe Text -> (SourcePos, SourcePos) -> Text -> Diagnostic
mkDiagnostic sev src (s, e) msg = Diagnostic range (Just sev) Nothing src msg Nothing Nothing
  where
    range :: Range
    range =
      mkRange
        (fromIntegral (Pos.unPos s.sourceLine) - 2)
        (fromIntegral (Pos.unPos s.sourceColumn) - 1)
        (fromIntegral (Pos.unPos e.sourceLine) - 2)
        $ fromIntegral (Pos.unPos e.sourceColumn) - 1
