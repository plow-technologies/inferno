{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Inferno.LSP.Completion where

import Data.Bifunctor (bimap)
import Data.List (delete, nub)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Inferno.LSP.ParseInfer (getTypeMetadataText, mkPrettyTy)
import Inferno.Types.Syntax (Ident (..), ModuleName (..), TypeClass, rws)
import Inferno.Types.Type (Namespace (..), TCScheme, TypeMetadata (..))
import Inferno.Utils.Prettyprinter (renderDoc, renderPretty)
import Language.LSP.Types
  ( CompletionDoc (..),
    CompletionItem (..),
    CompletionItemKind (..),
    MarkupContent (MarkupContent),
    MarkupKind (..),
    Position (..),
  )
import Prettyprinter (Pretty)

-- | Given the cursor position construct the corresponding 'completion query'
-- consisting of the leadup, i.e. text leading up to the word prefix that is to
-- be completed, as well as the prefix that is to be completed.
completionQueryAt :: Text -> Position -> (Text, Text)
completionQueryAt text pos = (completionLeadup, completionPrefix)
  where
    off = positionToOffset text pos
    text' = Text.take off text
    breakEnd :: (Char -> Bool) -> Text -> (Text, Text)
    breakEnd p =
      bimap Text.reverse Text.reverse . Text.break p . Text.reverse
    (completionPrefix, completionLeadup) =
      breakEnd (`elem` (" \t\n[(,=+*&|}?>" :: String)) text'

    positionToOffset :: Text -> Position -> Int
    positionToOffset txt (Position line col) =
      if fromIntegral line < length ls
        then Text.length . unlines' $ take (fromIntegral line) ls ++ [Text.take (fromIntegral col) (ls !! fromIntegral line)]
        else Text.length txt -- position lies outside txt
      where
        ls = NonEmpty.toList (lines' txt)

    lines' :: Text -> NonEmpty.NonEmpty Text
    lines' t =
      case Text.split (== '\n') t of
        [] -> "" NonEmpty.:| [] -- this case never occurs!
        l : ls -> l NonEmpty.:| ls
    unlines' :: [Text] -> Text
    unlines' = Text.intercalate "\n"

findInPrelude :: forall c. (Pretty c, Eq c) => Map.Map (Maybe ModuleName, Namespace) (TypeMetadata TCScheme) -> Text -> [((Maybe ModuleName, Namespace), TypeMetadata TCScheme)]
findInPrelude preludeNameToTypeMap prefix =
  let prefixIsEnum = "#" `Text.isPrefixOf` prefix
      -- 'preludeNameToTypeMap' stores the enum's ident without '#'.
      -- When comparing the prefix, we have to drop the '#'.
      lcPrefix' = (if prefixIsEnum then Text.drop 1 else id) $ Text.toLower prefix
      lcPrefix = (if "." `Text.isSuffixOf` lcPrefix' then Text.dropEnd 1 else id) lcPrefix'
   in Map.toList $
        Map.filterWithKey
          ( \(mModule, ns) _ ->
              case mModule of
                _ | prefixIsEnum -> filterEnum ns lcPrefix
                Just m ->
                  lcPrefix
                    `Text.isPrefixOf` Text.toLower (Text.toLower $ unModuleName m)
                    || filterNs ns lcPrefix
                    || filterNsWithModuleName m ns lcPrefix
                -- In the case of enum, we should only show enums
                Nothing -> filterNs ns lcPrefix
          )
          preludeNameToTypeMap
  where
    filterEnum ns lcPrefix = case ns of
      EnumNamespace (Ident i) -> lcPrefix `Text.isPrefixOf` Text.toLower i
      _ -> False

    filterNs ns lcPrefix = case ns of
      FunNamespace (Ident i) -> lcPrefix `Text.isPrefixOf` Text.toLower i
      OpNamespace (Ident i) -> lcPrefix `Text.isPrefixOf` Text.toLower i
      EnumNamespace (Ident i) -> lcPrefix `Text.isPrefixOf` Text.toLower i
      ModuleNamespace (ModuleName n) -> lcPrefix `Text.isPrefixOf` Text.toLower n
      TypeNamespace _ -> False

    filterNsWithModuleName (ModuleName mn) ns lcPrefix = do
      let mn' = Text.append (Text.toLower mn) "."
      case ns of
        FunNamespace (Ident i) -> lcPrefix `Text.isPrefixOf` Text.append mn' (Text.toLower i)
        OpNamespace (Ident i) -> lcPrefix `Text.isPrefixOf` Text.append mn' (Text.toLower i)
        EnumNamespace (Ident i) -> lcPrefix `Text.isPrefixOf` Text.append mn' (Text.toLower i)
        ModuleNamespace (ModuleName n) -> lcPrefix `Text.isPrefixOf` Text.append mn' (Text.toLower n)
        TypeNamespace _ -> False

mkCompletionItem :: Set TypeClass -> Text -> (Maybe ModuleName, Namespace) -> TypeMetadata TCScheme -> CompletionItem
mkCompletionItem typeClasses txt (modNm, ns) tm@TypeMetadata{ty} =
  CompletionItem
    { _label = insertModNm $ renderPretty ns
    , _kind = case ns of
        FunNamespace _ -> Just CiFunction
        OpNamespace _ -> Just CiFunction
        EnumNamespace _ -> Just CiEnum
        ModuleNamespace _ -> Just CiModule
        TypeNamespace _ -> Nothing
    , _tags = Nothing
    , _detail = Just $ renderDoc $ mkPrettyTy typeClasses mempty ty
    , _documentation = CompletionDocMarkup . MarkupContent MkMarkdown <$> getTypeMetadataText tm
    , _deprecated = Nothing
    , _preselect = Nothing
    , _sortText = Nothing
    , _filterText =
        let ftxt = insertModNm $ renderPretty ns
            ftxt' = (if "#" `Text.isPrefixOf` ftxt then Text.drop 1 else id) ftxt
         in Just ftxt'
    , _insertText = case ns of
        EnumNamespace (Ident i) -> if "#" `Text.isPrefixOf` txt then Just i else Nothing
        _ -> Nothing
    , _insertTextMode = Nothing
    , _insertTextFormat = Nothing
    , _textEdit = Nothing
    , _additionalTextEdits = Nothing
    , _commitCharacters = Nothing
    , _command = Nothing
    , _xdata = Nothing
    }
  where
    insertModNm txt' = case modNm of
      Nothing -> txt'
      Just (ModuleName n) -> do
        let moduleName = n <> "."
        if moduleName `Text.isPrefixOf` txt
          then txt'
          else moduleName <> txt'

-- | Create completion for user provided identifier e.g. input0, etc
identifierCompletionItems :: [Text] -> Text -> [CompletionItem]
identifierCompletionItems idents prefix
  | "." `Text.isSuffixOf` prefix = [] -- For case like "Module.", returned empty because identifier has no namespace/module prefix
  | otherwise = makeIdentifierCompletion <$> filter (\identifier -> prefix `Text.isPrefixOf` identifier) idents
  where
    makeIdentifierCompletion identifier =
      CompletionItem
        { _label = identifier
        , _kind = Just CiVariable
        , _tags = Nothing
        , _detail = Nothing
        , _documentation = Nothing
        , _deprecated = Nothing
        , _preselect = Nothing
        , _sortText = Nothing
        , _filterText = Just identifier
        , _insertText = Nothing
        , _insertTextMode = Nothing
        , _insertTextFormat = Nothing
        , _textEdit = Nothing
        , _additionalTextEdits = Nothing
        , _commitCharacters = Nothing
        , _command = Nothing
        , _xdata = Nothing
        }

rwsCompletionItems :: Text -> [CompletionItem]
rwsCompletionItems prefix
  | "." `Text.isSuffixOf` prefix = [] -- For case like "Module.", returned empty because identifier has no namespace/module prefix
  | otherwise = map mkRwsCompletionItem $ filter (\rw -> prefix `Text.isPrefixOf` rw) filteredRws
  where
    -- `None` and `Some` are already included elsewhere
    filteredRws :: [Text]
    filteredRws = delete "Some" $ delete "None" rws

    -- Create a CompletionItem for each reserved word
    mkRwsCompletionItem :: Text -> CompletionItem
    mkRwsCompletionItem rw =
      CompletionItem
        { _label = rw
        , _kind = Just CiKeyword
        , _tags = Nothing
        , _detail = Nothing
        , _documentation = Nothing
        , _deprecated = Nothing
        , _preselect = Nothing
        , _sortText = Nothing
        , _filterText = Nothing
        , _insertText = Nothing
        , _insertTextMode = Nothing
        , _insertTextFormat = Nothing
        , _textEdit = Nothing
        , _additionalTextEdits = Nothing
        , _commitCharacters = Nothing
        , _command = Nothing
        , _xdata = Nothing
        }

moduleNameCompletionItems :: forall c. (Pretty c, Eq c) => Map.Map (Maybe ModuleName, Namespace) (TypeMetadata TCScheme) -> [CompletionItem]
moduleNameCompletionItems preludeNameToTypeMap = fmap mkModuleCompletionItem modules
  where
    modules = nub . fmap unModuleName . mapMaybe fst $ Map.keys preludeNameToTypeMap
    mkModuleCompletionItem m =
      CompletionItem
        { _label = m
        , _kind = Just CiModule
        , _tags = Nothing
        , _detail = Nothing
        , _documentation = Nothing
        , _deprecated = Nothing
        , _preselect = Nothing
        , _sortText = Nothing
        , _filterText = Just m
        , _insertText = Just m
        , _insertTextMode = Nothing
        , _insertTextFormat = Nothing
        , _textEdit = Nothing
        , _additionalTextEdits = Nothing
        , _commitCharacters = Nothing
        , _command = Nothing
        , _xdata = Nothing
        }

filterModuleNameCompletionItems :: forall c. (Pretty c, Eq c) => Map.Map (Maybe ModuleName, Namespace) (TypeMetadata TCScheme) -> Text -> [CompletionItem]
filterModuleNameCompletionItems preludeNameToTypeMap prefix = filter (\item -> prefix `Text.isPrefixOf` _label item) (moduleNameCompletionItems @c preludeNameToTypeMap)
