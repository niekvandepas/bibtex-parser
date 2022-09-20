module Parse where
import Text.Parsec (spaces, parserTrace)
import Text.ParserCombinators.Parsec
import Entry (Entry, fromFields)
import Field (Field)
import BibtexType (BibtexType)
import Control.Monad (liftM)
import Data.Map (fromList)
import Data.Maybe (catMaybes)

parseBibtex :: String -> Either ParseError [Entry]
parseBibtex input = parse parseBibtexFile "bibtex" input

parseBibtexFile :: Parser [Entry]
parseBibtexFile = many parseEntry

parseEntry :: Parser (Entry)
parseEntry = do
  char '@'
  bibtexType <- parseBibtexType
  (key, fields) <- between (char '{') (char '}') $ do
    key <- parseKey
    fields <- endBy parseField (string ",\n")
    -- trace ("\nKey: " ++ key ++ "\n") $ return (key, fromList fields)
    return (key, fromList fields)
  return $ fromFields fields bibtexType key
  -- trace (show key ++ "\n\n" ++ show fields) $ undefined

-- Note to self: '>>' is 'then', '<|>' is 'or'.
parseField :: Parser (Field, String)
parseField = do
  -- field <- trace "starting read " $ spaces >> readField
  field <- spaces >> readField
  value <- spaces >> char '=' >> spaces >> between (char '{') (char '}') (many1 $ noneOf "}")
  return (field, value)
  -- trace ("parseField: " ++ show field ++ " = " ++ value) $ return (field, value)

readField :: Parser Field
readField = liftM read $ many1 alphaNum

parseKey :: Parser String
parseKey = manyTill anyChar $ char ','

parseBibtexType :: Parser BibtexType
parseBibtexType = liftM read $ many1 alphaNum
