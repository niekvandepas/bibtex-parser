module Main where
import           Brick           (simpleMain)
import           Control.Monad   (liftM)
import           Data.Char       (toLower, toUpper)
import           Data.List       (isPrefixOf)
import           Data.Map        (fromList)
import           Data.Map.Lazy   (Map)
import           Parse.Bibtex    (parseBibtex)
import           Text.Megaparsec (errorBundlePretty)
import           UI              (ui)

main :: IO ()
main = do
  s <- readFile "testfile.bib"
  case (parseBibtex . preProcess) s of
    Left p        -> putStrLn (errorBundlePretty p)
    Right entries -> simpleMain $ ui entries

-- | Strips consecutive newlines
preProcess :: String -> String
preProcess = unlines . filter (/= "") . lines
