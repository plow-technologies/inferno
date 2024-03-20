{-# LANGUAGE TemplateHaskellQuotes #-}

module Inferno.Utils.QQ.Common where

import Data.Text (Text)
import qualified Data.Text as Text
import Inferno.Parse.Error (prettyError)
import Language.Haskell.TH.Syntax
  ( Exp (AppE, VarE),
    Lift (lift),
    Loc (loc_filename, loc_start),
    Q,
    location,
  )
import Text.Megaparsec (ParseError, ShowErrorComponent, SourcePos (..), mkPos, unPos)

location' :: Q SourcePos
location' = aux <$> location
  where
    aux :: Loc -> SourcePos
    aux loc = let (l, c) = loc_start loc in SourcePos (loc_filename loc) (mkPos l) (mkPos c)

-- fix for https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable
liftText :: Text -> Q Exp
liftText txt = AppE (VarE 'Text.pack) <$> lift (Text.unpack txt)

mkParseErrorStr :: ShowErrorComponent e => (ParseError Text e, SourcePos) -> String
mkParseErrorStr (err, SourcePos {..}) =
  "Error at line "
    <> show (unPos sourceLine)
    <> " column "
    <> show (unPos sourceColumn)
    <> "\n        "
    <> Text.unpack (Text.replace "\n" "\n        " $ Text.pack $ prettyError err)
