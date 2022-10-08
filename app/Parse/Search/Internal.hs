{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RecordWildCards #-}
module Parse.Search.Internal (parseSearch, search) where
import Field (Field (..))
import Data.List.Split (splitOn, splitWhen)
import Text.Read (readMaybe)
import Data.Char (toLower, isSpace)
import Data.Map (Map, fromList, toList)
import Entry (Entry (..))

search :: String -> [Entry] -> [Entry]
search s entries = searchEntries (parseSearch s) entries

parseSearch :: String -> Map Field (Maybe String)
parseSearch s = fromList $ map readParsed (keysValues s)
  where
    params :: [String]
    params = splitOn ";" s
    keysValues s = map (splitAtPredicate (== '=')) $ params
    readParsed :: (String, String) -> (Field, Maybe String)
    readParsed (k, "") = (read (trim k), Nothing)
    readParsed (k, v) = (read (trim k), Just (trim v))

searchEntries :: Map Field (Maybe String) -> [Entry] -> [Entry]
searchEntries filters = filter (keep filters)
  where
    keep :: Map Field (Maybe String) -> Entry -> Bool
    keep filters entry = mapAll (\filter -> verify filter entry) filters
    mapAll :: ((k, v) -> Bool) -> Map k v -> Bool
    mapAll f m = all f (toList m)
    verify :: (Field, Maybe String) -> Entry -> Bool
    -- Any entry will match an empty filter
    verify (field, Nothing) entry       = True
    verify (Address, value) entry       = (show $ address entry) `strComp` value
    verify (Abstract, value) entry      = (show $ abstract entry) `strComp` value
    verify (Annote, value) entry        = (show $ annote entry) `strComp` value
    verify (Author, value) entry        = maybe False (`elem` author entry) value
    verify (Booktitle, value) entry     = (show $ booktitle entry) `strComp` value
    verify (Chapter, value) entry       = (show $ chapter entry) `strComp` value
    verify (Crossref, value) entry      = (show $ crossref entry) `strComp` value
    verify (DOI, value) entry           = (show $ doi entry) `strComp` value
    verify (Edition, value) entry       = (show $ edition entry) `strComp` value
    verify (Editor, value) entry        = (show $ editor entry) `strComp` value
    verify (Howpublished, value) entry  = (show $ howpublished entry) `strComp` value
    verify (Institution, value) entry   = (show $ institution entry) `strComp` value
    verify (ISSN, value) entry          = (show $ issn entry) `strComp` value
    verify (Issue, value) entry         = (show $ issue entry) `strComp` value
    verify (Journal, value) entry       = (show $ journal entry) `strComp` value
    verify (Keywords, value) entry      = (show $ keywords entry) `strComp` value
    verify (Month, value) entry         = (show $ month entry) `strComp` value
    verify (Note, value) entry          = (show $ note entry) `strComp` value
    verify (Number, value) entry        = (show $ number entry) `strComp` value
    verify (Organization, value) entry  = (show $ organization entry) `strComp` value
    verify (Pages, value) entry         = (show $ pages entry) `strComp` value
    verify (Publisher, value) entry     = (show $ publisher entry) `strComp` value
    verify (School, value) entry        = (show $ school entry) `strComp` value
    verify (Series, value) entry        = (show $ series entry) `strComp` value
    verify (Title, value) entry         = (show $ title entry) `strComp` value
    verify (Type, value) entry          = (show $ reporttype entry) `strComp` value
    verify (Volume, value) entry        = (show $ volume entry) `strComp` value
    verify (Year, value) entry          = (show $ year entry) `strComp` value

-- | 'splitAtPredicate', applied to a predicate @p@ and a list @xs@,
-- splits the list at element @x@ satisfying @p@, dropping @x@.
-- If 'p' is not statisfied, the full list and the null list are returned as a tuple.
splitAtPredicate :: (a -> Bool) -> [a] -> ([a], [a])
splitAtPredicate _ [] = ([], [])
splitAtPredicate p xs = splitAtPredicateAcc p [] xs
  where
    splitAtPredicateAcc p' left right@(x : xs')
      | null right = (left, right)
      | p' x = (left, xs')
      | otherwise = splitAtPredicateAcc p' (left ++ [x]) xs'

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

strComp :: String -> Maybe String -> Bool
strComp s Nothing = False
strComp s (Just s') = s == s'
