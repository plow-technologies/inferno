module Inferno.Parse.Error where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEList
import Data.Maybe (isNothing)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, replace, unpack)
import Text.Megaparsec
  ( ErrorFancy (..),
    ErrorItem (..),
    ParseError (..),
    ShowErrorComponent (..),
    Stream (Token),
    unPos,
  )
import Text.Megaparsec.Stream (showTokens)

drop_ :: String -> String
drop_ "" = ""
drop_ ('_' : xs) = xs
drop_ xs = xs

orList :: NonEmpty Text -> String
orList (x :| []) = drop_ $ unpack x
orList xs =
  "one of the following:\n∙ "
    <> intercalate "\n∙ " (map (drop_ . unpack . replace "\n" "\n  ") $ NEList.toList xs)

showErrorItem :: ErrorItem (Token Text) -> String
showErrorItem = \case
  Tokens ts -> showTokens (Proxy :: Proxy Text) ts
  Label lbl -> NEList.toList lbl
  EndOfInput -> "end of input"

messageItemsPretty ::
  -- | Prefix to prepend
  String ->
  -- | Collection of messages
  Set String ->
  -- | Result of rendering
  String
messageItemsPretty prefix ts
  | Set.null ts = ""
  | otherwise =
      prefix <> (orList . NEList.fromList . Set.toAscList . Set.map pack) ts <> "\n"

showErrorFancy :: (ShowErrorComponent e) => ErrorFancy e -> String
showErrorFancy = \case
  ErrorFail msg -> msg
  ErrorIndentation ord ref actual ->
    "incorrect indentation (got "
      <> show (unPos actual)
      <> ", should be "
      <> p
      <> show (unPos ref)
      <> ")"
    where
      p = case ord of
        LT -> "less than "
        EQ -> "equal to "
        GT -> "greater than "
  ErrorCustom a -> showErrorComponent a

prettyError ::
  (ShowErrorComponent e) =>
  -- | Parse error to render
  ParseError Text e ->
  String
prettyError (TrivialError _ us ps) =
  if isNothing us && Set.null ps
    then "unknown parse error\n"
    else
      messageItemsPretty "unexpected " (showErrorItem `Set.map` maybe Set.empty Set.singleton us)
        <> messageItemsPretty "expecting " (showErrorItem `Set.map` ps)
prettyError (FancyError _ xs) =
  if Set.null xs
    then "unknown fancy parse error\n"
    else unlines (showErrorFancy <$> Set.toAscList xs)
