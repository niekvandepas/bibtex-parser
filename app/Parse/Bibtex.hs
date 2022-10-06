{-# LANGUAGE RecordWildCards #-}
module Parse.Bibtex (parseBibtex) where
import Entry (Entry (..), fromFields)
import Field (Field (Author))
import BibtexType (BibtexType)
import Control.Monad (liftM)
import Data.Map (fromList, Map, lookup)
import Data.Maybe (catMaybes)
import Data.List.Split (splitOn)
import Text.Megaparsec (Parsec, ParseError, ParseErrorBundle, parse, many, between, endBy, noneOf, satisfy, parseTest, some, manyTill, choice, (<?>), anySingleBut)
import Text.Megaparsec.Char (char, string, space, alphaNumChar, printChar)
import Text.ParserCombinators.ReadP (many1)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Debug.Trace (trace)

type Parser = Parsec () String

parseBibtex :: String -> Either (ParseErrorBundle String ()) [Entry]
parseBibtex input = parse parseBibtexFile "bibtex" input
-- parseBibtex _ = trace (show $ parse p "test" "{hello world}") $ Right []
--   where
--     p :: Parser String
--     p = do
--      char '{'
--      some (anySingleBut '}')

parseBibtexFile :: Parser [Entry]
parseBibtexFile = many parseEntry

parseEntry :: Parser (Entry)
parseEntry = do
  char '@'
  bibtexType <- parseBibtexType
  (key, fields) <- parseFields
  case Data.Map.lookup Author fields of
    Nothing -> fail "No 'author' field"
    Just authors -> return $ fromFields fields (splitOn " and " authors) bibtexType key

parseFields :: Parser (String, Map Field String)
parseFields = between (char '{') (char '}') $ do
  key <- parseKey <?> "valid key"
  char '\n'
  fields <- trace (key) endBy parseField (string "},\n")
  return (key, fromList fields)

parseField :: Parser (Field, String)
parseField = do
  field <- space >> choice validFields <?> "valid field"
  value <- space >> char '=' >> space >> char '{' >> some (anySingleBut '}') <?> ("value for field " ++ field)
  return (read field, value)
  where
    validFields =
      [ string "abstract"
      , string "annote"
      , string "address"
      , string "author"
      , string "booktitle"
      , string "chapter"
      , string "crossref"
      , string "doi"
      , string "edition"
      , string "editor"
      , string "howpublished"
      , string "institution"
      , string "issn"
      , string "issue"
      , string "journal"
      , string "keywords"
      , string "month"
      , string "note"
      , string "number"
      , string "organization"
      , string "pages"
      , string "publisher"
      , string "school"
      , string "series"
      , string "title"
      , string "type"
      , string "volume"
      , string "year"
      ]

parseKey :: Parser String
parseKey = (manyTill charLiteral $ char ',') <?> "key"

parseBibtexType :: Parser BibtexType
parseBibtexType = (liftM read $ some alphaNumChar) <?> "type"

splitAuthors :: Maybe String -> [String]
splitAuthors Nothing = []
splitAuthors (Just s) = splitOn " and " s
