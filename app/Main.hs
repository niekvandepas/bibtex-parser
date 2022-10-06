module Main where
import System.Environment
import Control.Monad (liftM)
import Data.Char (toUpper, toLower)
import Data.Map (fromList)
import Data.List (isPrefixOf)
import Data.Map.Lazy (Map)
import Parse.Bibtex (parseBibtex)

main :: IO ()
main = do
  putStr "\n\n" -- Seperator between compiler and program output
  s <- readFile "testfile.bib"
  (print . parseBibtex . preProcess) s

-- | Strips consecutive newlines
preProcess :: String -> String
preProcess = unlines . filter (/= "") . lines

capitalize :: String -> String
capitalize (x:xs) = toUpper x : (map toLower xs)
capitalize [] = []
