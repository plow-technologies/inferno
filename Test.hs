module Test where

import qualified Data.Text as T

foo :: String
-- foo = show 3.14
foo = show $ T.unpack (T.pack "\55555")

-- import Inferno.Types.Syntax

-- foo :: String
-- foo = show $ LDouble 3.14
