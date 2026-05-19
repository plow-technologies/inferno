{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Inferno.LSP.Hover
  ( HoverIndex,
    HoverEntry (HoverEntry, meta, start, end),
    linearize,
    buildHoverIndex,
    queryHover,
    renderHoverContent,
  ) where

import Data.IntervalMap.Generic.Strict (IntervalMap)
import qualified Data.IntervalMap.Generic.Strict as IntervalMap
import Data.IntervalMap.Interval (Interval)
import qualified Data.IntervalMap.Interval as Interval
import Data.List.Extra (nubOrd)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Inferno.Infer (findTypeClassWitnesses)
import Inferno.Types.Syntax
  ( BaseType (TEnum),
    Ident (unIdent),
    ImplType (ImplType),
    InfernoType (TBase, TRep),
    Substitutable (ftv),
    TCScheme,
    TV,
    TypeClass (TypeClass),
    TypeMetadata,
    punctuate',
    substMap,
  )
import qualified Inferno.Types.Syntax
import Inferno.Utils.Prettyprinter (renderDoc)
import Language.LSP.Types
  ( MarkupContent (MarkupContent),
    MarkupKind (MkMarkdown),
    Range,
    UInt,
    mkRange,
  )
import Prettyprinter (Doc, Pretty (pretty), align, hardline, sep, (<+>))
import Text.Megaparsec.Pos (SourcePos)
import qualified Text.Megaparsec.Pos as Pos

-- | A single entry in the hover index, pairing type metadata with its
-- source span.
data HoverEntry = HoverEntry
  { meta :: !(TypeMetadata TCScheme)
  , start :: !SourcePos
  , end :: !SourcePos
  }

-- | Interval map from linearized source positions to hover entries. Supports
-- O(log n) point queries via @IntervalMap.containing@. See 'linearize' for the
-- encoding scheme.
type HoverIndex = IntervalMap (Interval Int) HoverEntry

-- | Encode a 'SourcePos' as a single @Int@ for use as an @IntervalMap@ key.
-- Maps 2D @(line, col)@ to a single @Int@ via @line * 10000 + col@. This
-- preserves containment: if span A contains span B, then
-- @linearize A.start <= linearize B.start@ and
-- @linearize A.end >= linearize B.end@. The fixed column width of @10000@ is
-- safe for any realistic Inferno script.
--
-- Also used as the key for @DocState.hoverCache@ (@IntMap@), so hover cache
-- lookups use the same encoding as index queries.
linearize :: SourcePos -> Int
linearize pos =
  (Pos.unPos pos.sourceLine - 1) * 10000 + (Pos.unPos pos.sourceColumn - 1)

-- | Build an interval map from the type map produced by the inferencer. Each
-- source span is linearized into a closed @Int@ interval.
buildHoverIndex :: Map (SourcePos, SourcePos) (TypeMetadata TCScheme) -> HoverIndex
buildHoverIndex = IntervalMap.fromList . fmap toEntry . Map.toList
  where
    toEntry :: ((SourcePos, SourcePos), TypeMetadata TCScheme) -> (Interval Int, HoverEntry)
    toEntry ((s, e), m) = (Interval.ClosedInterval (linearize s) (linearize e), HoverEntry m s e)

-- | Query the hover index at a given LSP @(line, col)@ position. Finds all
-- intervals containing the linearized point, then picks the smallest
-- (innermost) span. Returns 'Nothing' if the position has no type information.
queryHover :: UInt -> UInt -> HoverIndex -> Maybe HoverEntry
queryHover line col = smallestContaining . flip IntervalMap.containing pt
  where
    pt :: Int
    pt = fromIntegral line * 10000 + fromIntegral col

-- | Find the entry with the smallest enclosing interval (i.e. the most
-- specific/innermost type). Folds the map directly; no intermediate list.
smallestContaining :: IntervalMap (Interval Int) a -> Maybe a
smallestContaining =
  fmap snd . IntervalMap.foldlWithKey pick Nothing
  where
    pick :: Maybe (Interval Int, a) -> Interval Int -> a -> Maybe (Interval Int, a)
    pick = \cases
      Nothing k v -> Just (k, v)
      (Just acc@(a, _)) k v
        | ivSpan k < ivSpan a -> Just (k, v)
        | otherwise -> Just acc

    ivSpan :: Interval Int -> Int
    ivSpan iv = Interval.upperBound iv - Interval.lowerBound iv

-- | Render a hover entry into an LSP 'Range' and markdown 'MarkupContent'.
-- The @- 2@ offset on source lines accounts for the 2-line @fun ... ->@ prefix
-- prepended before parsing.
renderHoverContent :: Set TypeClass -> Set TypeClass -> HoverEntry -> (Range, MarkupContent)
renderHoverContent allClasses docClasses entry =
  (range, MarkupContent MkMarkdown content)
  where
    prettyTy :: Doc ()
    prettyTy = mkPrettyTy allClasses docClasses entry.meta.ty

    range :: Range
    range =
      mkRange
        (fromIntegral (Pos.unPos entry.start.sourceLine) - 2)
        (fromIntegral (Pos.unPos entry.start.sourceColumn) - 1)
        (fromIntegral (Pos.unPos entry.end.sourceLine) - 2)
        $ fromIntegral (Pos.unPos entry.end.sourceColumn) - 1

    content :: Text
    content =
      mconcat
        [ "**Type**\n"
        , "~~~inferno\n"
        , renderDoc (pretty entry.meta.identExpr <+> align prettyTy)
        , "\n~~~"
        , maybe "" ("\n" <>) (metadataDocsText entry.meta)
        ]

-- | Pretty-print a type scheme as a union signature. If the scheme has free
-- type variables, resolves typeclass witnesses (capped at @11@) to produce
-- concrete instantiations. Displays up to @10@ variants; truncates with @...@.
mkPrettyTy :: Set TypeClass -> Set TypeClass -> TCScheme -> Doc ann
mkPrettyTy allClasses docClasses sch
  | Set.null ftvTy = ":" <+> align (pretty sch.impl)
  | null subs = ":" <+> align (pretty sch.impl)
  | null (drop 10 prettyList) = sep $ unionTySig prettyList
  | otherwise = sep . unionTySig $ take 10 prettyList <> ["..."]
  where
    ftvTy :: Set TV
    ftvTy = ftv sch.impl

    filteredCls :: Set TypeClass
    filteredCls = Set.filter (not . isRepClass) $ Set.union sch.classes docClasses

    subs :: [Map TV InfernoType]
    subs = findTypeClassWitnesses allClasses (Just 11) filteredCls ftvTy

    filtered :: ImplType
    filtered = filterImplReps sch.impl

    applyWit :: Map TV InfernoType -> ImplType
    applyWit m =
      ImplType (fmap (substMap m) filtered.impl) $ substMap m filtered.body

    prettyList :: [Doc ann]
    prettyList = fmap pretty . nubOrd $ fmap applyWit subs

unionTySig :: [Doc ann] -> [Doc ann]
unionTySig = \case
  [] -> []
  (t : ts) -> (":" <+> t) : fmap ("|" <+>) ts

isRepClass :: TypeClass -> Bool
isRepClass = \case
  TypeClass "rep" _ -> True
  _ -> False

-- | Remove 'TRep' entries from an 'ImplType'\'s implicit context; these are
-- runtime representation constraints that should not appear in hover output.
filterImplReps :: ImplType -> ImplType
filterImplReps (ImplType impls ty) =
  flip ImplType ty $ Map.filter (not . isRep) impls
  where
    isRep :: InfernoType -> Bool
    isRep = \case
      TRep _ -> True
      _ -> False

-- | Extract documentation text from type metadata. For enum types, appends
-- a fenced code block showing the enum definition.
metadataDocsText :: TypeMetadata TCScheme -> Maybe Text
metadataDocsText meta
  | Nothing <- meta.docs = Nothing
  | Just d <- meta.docs
  , TBase (TEnum nm cs) <- meta.ty.impl.body =
      Just . renderDoc $
        mconcat
          [ pretty d
          , hardline
          , "~~~inferno"
          , hardline
          , "enum"
              <+> pretty nm
              <+> align
                ( sep
                    ( "="
                        : punctuate'
                          "|"
                          (("#" <>) . pretty . unIdent <$> Set.toList cs)
                    )
                )
          , hardline
          , "~~~"
          ]
  | otherwise = renderDoc . pretty <$> meta.docs
