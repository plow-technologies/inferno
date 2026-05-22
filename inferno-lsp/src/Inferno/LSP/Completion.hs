{-# LANGUAGE NoFieldSelectors #-}

module Inferno.LSP.Completion () where

import Data.Bool (bool)
import Data.Function ((&))
import Data.List (scanl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Inferno.LSP.Hover (metadataDocsText, mkPrettyTy)
import Inferno.Types.Syntax
  ( Ident (Ident),
    ModuleName (ModuleName),
    Namespace (EnumNamespace, FunNamespace, ModuleNamespace, OpNamespace, TypeNamespace),
    TypeClass,
  )
import qualified Inferno.Types.Syntax
import Inferno.Types.Type (TCScheme, TypeMetadata)
import Inferno.Utils.Prettyprinter (renderDoc, renderPretty)
import qualified Language.LSP.Types as LSP

-- | Given a cursor @Position@, split the document text into a
-- @(leadup, prefix)@ pair where @prefix@ is the token fragment being
-- completed (everything back to the nearest break character) and @leadup@
-- is the text preceding it.
completionQueryAt :: Text -> LSP.Position -> (Text, Text)
completionQueryAt txt pos = (leadup, prefix)
  where
    truncated :: Text
    truncated = flip Text.take txt $ positionToOffset txt pos

    isBreak :: Char -> Bool
    isBreak =
      (`elem` [' ', '\t', '\n', '[', '(', ',', '=', '+', '*', '&', '|', '}', '?', '>'])

    prefix :: Text
    prefix = Text.takeWhileEnd (not . isBreak) truncated

    leadup :: Text
    leadup = Text.dropEnd (Text.length prefix) truncated

-- | Filter the prelude map to entries matching a completion @prefix@. If the
-- prefix starts with @#@, only enum constructors are considered. Otherwise
-- matches against unqualified names, module names, and qualified @Module.name@
-- forms.
--
-- Returns a filtered sub-map; callers needing an association list should apply
-- @Map.toList@ at the call site.
findInPrelude ::
  Map (Maybe ModuleName, Namespace) (TypeMetadata TCScheme) ->
  Text ->
  Map (Maybe ModuleName, Namespace) (TypeMetadata TCScheme)
findInPrelude prelude prefix = Map.filterWithKey matches prelude
  where
    matches :: (Maybe ModuleName, Namespace) -> TypeMetadata TCScheme -> Bool
    matches (mMod, ns) _ =
      ns & \case
        TypeNamespace _ -> False
        EnumNamespace ident
          | prefixIsEnum -> lcPrefix `Text.isPrefixOf` Text.toLower ident.unIdent
        _ | prefixIsEnum -> False
        _ -> any (lcPrefix `Text.isPrefixOf`) searchTexts
      where
        lcPrefix :: Text
        lcPrefix =
          Text.dropWhileEnd (== '.')
            . bool id (Text.drop 1) prefixIsEnum
            $ Text.toLower prefix

        prefixIsEnum :: Bool
        prefixIsEnum = "#" `Text.isPrefixOf` prefix

        searchTexts :: [Text]
        searchTexts =
          mMod & \case
            Nothing -> [renderNameSpace ns]
            Just modName ->
              [ Text.toLower modName.unModuleName
              , renderNameSpace ns
              , Text.toLower modName.unModuleName <> "." <> renderNameSpace ns
              ]

        renderNameSpace :: Namespace -> Text
        renderNameSpace = \case
          FunNamespace ident -> Text.toLower ident.unIdent
          OpNamespace ident -> Text.toLower ident.unIdent
          EnumNamespace ident -> Text.toLower ident.unIdent
          ModuleNamespace modName -> Text.toLower modName.unModuleName
          TypeNamespace _ -> mempty

-- | Context shared across all completion items in a single response.
data CompletionCtx = CompletionCtx
  { classes :: !(Set TypeClass)
  , prefix  :: !Text
  }

-- | Build a @CompletionItem@ from a prelude entry. Qualifies the label with
-- the module name unless the completion prefix already includes it. Renders
-- the type signature as detail and metadata docs as markdown documentation.
mkCompletionItem ::
  CompletionCtx ->
  (Maybe ModuleName, Namespace) ->
  TypeMetadata TCScheme ->
  LSP.CompletionItem
mkCompletionItem ctx (modNm, ns) meta =
  LSP.CompletionItem
    { LSP._label = qualifiedLabel
    , LSP._kind = nsKind
    , LSP._tags = Nothing
    , LSP._detail = Just . renderDoc $ mkPrettyTy ctx.classes mempty meta.ty
    , LSP._documentation =
        LSP.CompletionDocMarkup . LSP.MarkupContent LSP.MkMarkdown
          <$> metadataDocsText meta
    , LSP._deprecated = Nothing
    , LSP._preselect = Nothing
    , LSP._sortText = Nothing
    , LSP._filterText =
        -- First checks if we are in enum namespace, which is faster than _always_
        -- doing a text scan for `#` (only enum idents can start with `#`)
        Just $ bool qualifiedLabel (Text.drop 1 qualifiedLabel) isEnum
    , LSP._insertText =
        ns & \case
          EnumNamespace ident ->
            bool Nothing (Just ident.unIdent) $ "#" `Text.isPrefixOf` ctx.prefix
          _ -> Nothing
    , LSP._insertTextMode = Nothing
    , LSP._insertTextFormat = Nothing
    , LSP._textEdit = Nothing
    , LSP._additionalTextEdits = Nothing
    , LSP._commitCharacters = Nothing
    , LSP._command = Nothing
    , LSP._xdata = Nothing
    }
  where
    qualifiedLabel :: Text
    qualifiedLabel =
      modNm & \case
        Nothing -> renderPretty ns
        Just modName ->
          bool (qual <> renderPretty ns) (renderPretty ns) $
            qual `Text.isPrefixOf` ctx.prefix
          where
            qual :: Text
            qual = modName.unModuleName <> "."

    isEnum :: Bool
    isEnum = case ns of
      EnumNamespace _ -> True
      _ -> False

    nsKind :: Maybe LSP.CompletionItemKind
    nsKind =
      ns & \case
        FunNamespace _ -> Just LSP.CiFunction
        OpNamespace _ -> Just LSP.CiFunction
        EnumNamespace _ -> Just LSP.CiEnum
        ModuleNamespace _ -> Just LSP.CiModule
        TypeNamespace _ -> Nothing

-- | Convert an LSP @Position@ (0-based line and column) to a 0-based
-- character offset into @txt@. Clamps to @Text.length txt@ if the position
-- lies beyond the end of the document.
positionToOffset :: Text -> LSP.Position -> Int
positionToOffset txt (LSP.Position line col) =
  min (lineStart + fromIntegral col) $ Text.length txt
  where
    lineStart :: Int
    lineStart =
      starts & drop (fromIntegral line) & \case
        (s : _) -> s
        [] -> Text.length txt

    starts :: [Int]
    starts = scanl' addLine 0 $ Text.splitOn "\n" txt

    addLine :: Int -> Text -> Int
    addLine acc l = acc + Text.length l + 1
