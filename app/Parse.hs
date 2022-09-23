{-# LANGUAGE RecordWildCards #-}
module Parse (parseBibtex) where
import Text.Parsec (spaces, parserTrace)
import Text.ParserCombinators.Parsec
import Entry (Entry (..), fromFields)
import Field (Field (Author))
import BibtexType (BibtexType)
import Control.Monad (liftM)
import Data.Map (fromList, Map, lookup)
import Data.Maybe (catMaybes)
import Data.List.Split (splitOn)

parseBibtex :: String -> Either ParseError [Entry]
parseBibtex input = parse parseBibtexFile "bibtex" input

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
    key <- parseKey
    fields <- endBy parseField (string ",\n")
    return (key, fromList fields)
      where
        parseField :: Parser (Field, String)
        parseField = do
          field <- spaces >> readField
          value <- spaces >> char '=' >> spaces >> between (char '{') (char '}') (many1 $ noneOf "}")
          return (field, value)

readField :: Parser Field
readField = liftM read $ many1 alphaNum

parseKey :: Parser String
parseKey = manyTill anyChar $ char ','

parseBibtexType :: Parser BibtexType
parseBibtexType = liftM read $ many1 alphaNum

splitAuthors :: Maybe String -> [String]
splitAuthors Nothing = []
splitAuthors (Just s) = splitOn " and " s
