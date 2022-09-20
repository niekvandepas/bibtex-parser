{-# LANGUAGE OverloadedStrings #-}
module BibtexType where
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Text (pack)

data BibtexType
  = Article
  | Book
  | Booklet
  | Inbook
  | Incollection
  | Inproceedings
  | Manual
  | Mastersthesis
  | Misc
  | Phdthesis
  | Proceedings
  | Techreport
  | Unpublished
  deriving (Eq, Show, Enum, Bounded)

instance Read BibtexType where
  readsPrec _ s
    | "article" `isPrefixOf` s' = [(Article, drop (length ("article" :: String)) s)]
    | "book" `isPrefixOf` s' = [(Book, drop (length ("book" :: String)) s)]
    | "booklet" `isPrefixOf` s' = [(Booklet, drop (length ("booklet" :: String)) s)]
    | "inbook" `isPrefixOf` s' = [(Inbook, drop (length ("inbook" :: String)) s)]
    | "incollection" `isPrefixOf` s' = [(Incollection, drop (length ("incollection" :: String)) s)]
    | "inproceedings" `isPrefixOf` s' = [(Inproceedings, drop (length ("inproceedings" :: String)) s)]
    | "manual" `isPrefixOf` s' = [(Manual, drop (length ("manual" :: String)) s)]
    | "mastersthesis" `isPrefixOf` s' = [(Mastersthesis, drop (length ("mastersthesis" :: String)) s)]
    | "misc" `isPrefixOf` s' = [(Misc, drop (length ("misc" :: String)) s)]
    | "phdthesis" `isPrefixOf` s' = [(Phdthesis, drop (length ("phdthesis" :: String)) s)]
    | "proceedings" `isPrefixOf` s' = [(Proceedings, drop (length ("proceedings" :: String)) s)]
    | "techreport" `isPrefixOf` s' = [(Techreport, drop (length ("techreport" :: String)) s)]
    | "unpublished" `isPrefixOf` s' = [(Unpublished, drop (length ("unpublished" :: String)) s)]
    | otherwise = error $ "read BibtexType: no parse for '" ++ s ++ "'"
      where s' = map toLower s
