module Inferno.Utils.Prettyprinter where

import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Text as Text (Text)
import qualified Data.Text.IO as Text (putStrLn)
import Prettyprinter
  ( Doc,
    LayoutOptions (LayoutOptions),
    PageWidth (AvailablePerLine),
    Pretty (..),
    layoutSmart,
  )
import Prettyprinter.Render.Text (renderStrict)

renderDoc :: Doc ann -> Text.Text
renderDoc = renderStrict . layoutSmart (LayoutOptions (AvailablePerLine 80 1))

renderPretty :: (Pretty a) => a -> Text.Text
renderPretty = renderDoc . pretty

showPretty :: (MonadIO m) => (Pretty a) => a -> m ()
showPretty = liftIO . Text.putStrLn . renderPretty
