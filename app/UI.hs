module UI (ui) where

import Data.Monoid ((<>))
import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center (center)
import Entry (Entry(..))
import Field (Field (..))
import Data.List (intercalate)
import UI.Internal (entriesTable)

ui :: [Entry] -> Widget ()
ui = center . renderTable . entriesTable
