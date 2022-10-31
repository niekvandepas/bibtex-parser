module UI.Internal (entriesTable) where
import Entry (Entry (..))
import Brick.Widgets.Table (Table, table)
import Brick (Widget, str)
import Data.List (intercalate)
import Field (Field(..))

entriesTable :: [Entry] -> Table ()
entriesTable entries =
    table $ headers ++ map entryRow entries

headers :: [[Widget ()]]
headers = [ map (str . show) [Author, Year, Title]]

entryRow :: Entry -> [Widget ()]
entryRow = (map str . columns)
  where
    columns e = [(showAuthors (author e)), (maybeShow (year e)), (maybeShow (title e))]
    showAuthors = intercalate ", "
    maybeShow :: (Show a) => Maybe a -> String
    maybeShow Nothing = ""
    maybeShow (Just x) = if length (show x) > 20 then (take 20 $ show x) ++ "…" else show x
