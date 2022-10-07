{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RecordWildCards #-}
module Parse.Search.Internal (parseSearch, search) where
import Field (Field)
import Data.List.Split (splitOn, splitWhen)
import Text.Read (readMaybe)
import Data.Char (toLower, isSpace)
import Data.Map (Map, fromList)

search :: ()
search = undefined

parseSearch :: String -> Map Field (Maybe String)
parseSearch s = fromList $ map readParsed (keysValues s)
  where
    params :: [String]
    params = splitOn ";" s
    keysValues s = map (splitAtPredicate (== '=')) $ params

readParsed :: (String, String) -> (Field, Maybe String)
readParsed (k, "") = (read (trim k), Nothing)
readParsed (k, v) = (read (trim k), Just (trim v))

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
