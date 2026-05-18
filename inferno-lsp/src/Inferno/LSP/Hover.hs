module Inferno.LSP.Hover
  ( HoverIndex,
    linearize,
  ) where

import Inferno.Types.Syntax (TCScheme, TypeMetadata)

import Data.IntervalMap.Generic.Strict (IntervalMap)
import Data.IntervalMap.Interval (Interval)
import Text.Megaparsec.Pos (SourcePos)
import qualified Text.Megaparsec.Pos as Pos

type HoverIndex =
  IntervalMap (Interval Int) (TypeMetadata TCScheme, SourcePos, SourcePos)

-- | Encode a `SourcePos` as a single `Int` for use as an `IntervalMap` key.
-- Uses a fixed column width of `10000`; safe for any realistic Inferno script.
linearize :: Pos.SourcePos -> Int
linearize pos =
  (Pos.unPos pos.sourceLine - 1) * 10000 + (Pos.unPos pos.sourceColumn - 1)
