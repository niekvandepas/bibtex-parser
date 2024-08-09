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
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  s <- readFile "testfile.bib"
  case (parseBibtex . preProcess) s of
    Left p        -> putStrLn (errorBundlePretty p)
    Right entries -> simpleMain $ ui entries

-- | Strips consecutive newlines
preProcess :: String -> String
preProcess = unlines . filter (/= "") . lines
