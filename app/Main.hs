module Main where
import System.Environment
import Control.Monad (liftM)
import Data.Char (toUpper, toLower)
import Data.Map (fromList)
import Data.List (isPrefixOf)
import Data.Map.Lazy (Map)
import Parse.Bibtex (parseBibtex)
import UI (ui)
import Brick (simpleMain)

main :: IO ()
main = do
  s <- readFile "testfile.bib"
  case (parseBibtex . preProcess) s of
    Left p -> print p
    Right entries -> simpleMain $ ui entries

-- | Strips consecutive newlines
preProcess :: String -> String
preProcess = unlines . filter (/= "") . lines
