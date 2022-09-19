{-# LANGUAGE OverloadedStrings #-}
module BibtexType where
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Text (pack)
import Database.SQLite.Simple.FromField (FromField, fromField, returnError)
import Database.SQLite.Simple.Internal (Field(..))
import Database.SQLite.Simple (SQLData(SQLText), ResultError (ConversionFailed))
import Database.SQLite.Simple.Ok ( Ok(Ok) )
import Database.SQLite.Simple.ToField (ToField (toField))

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

instance ToField BibtexType where
  toField = SQLText . Data.Text.pack . show

instance FromField BibtexType where
  fromField (Field (SQLText "Article") _) = Ok Article
  fromField (Field (SQLText "Book") _) = Ok Book
  fromField (Field (SQLText "Booklet") _) = Ok Booklet
  fromField (Field (SQLText "Inbook") _) = Ok Inbook
  fromField (Field (SQLText "Incollection") _) = Ok Incollection
  fromField (Field (SQLText "Inproceedings") _) = Ok Inproceedings
  fromField (Field (SQLText "Manual") _) = Ok Manual
  fromField (Field (SQLText "Mastersthesis") _) = Ok Mastersthesis
  fromField (Field (SQLText "Misc") _) = Ok Misc
  fromField (Field (SQLText "Phdthesis") _) = Ok Phdthesis
  fromField (Field (SQLText "Proceedings") _) = Ok Proceedings
  fromField (Field (SQLText "Techreport") _) = Ok Techreport
  fromField (Field (SQLText "Unpublished") _) = Ok Unpublished
  fromField f = returnError ConversionFailed f "BibtexType not valid"

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
