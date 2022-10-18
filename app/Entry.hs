{-# LANGUAGE RecordWildCards #-}
module Entry (Entry(..), fromFields, empty) where
import BibtexType (BibtexType)
import Data.Map (Map, lookup)
import Field (Field (..))
import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)

type Key = String
type Author = String

-- TODO it may be better to model `Entry` as follows:
-- data Entry = Article { name :: String, age :: Int } | Book { title :: String, publisher :: String} | ...etc.
data Entry = Entry
  { bibtexType :: BibtexType,
    key :: Key,
    abstract :: Maybe String,
    address :: Maybe String,
    annote :: Maybe String,
    author :: [Author],
    booktitle :: Maybe String,
    chapter :: Maybe String,
    crossref :: Maybe String,
    doi :: Maybe String,
    edition :: Maybe String,
    -- TODO model `editor` as [Author]
    editor :: Maybe String,
    howpublished :: Maybe String,
    institution :: Maybe String,
    issn :: Maybe String,
    issue :: Maybe String,
    journal :: Maybe String,
    keywords :: Maybe String,
    month :: Maybe String,
    note :: Maybe String,
    number :: Maybe String,
    organization :: Maybe String,
    pages :: Maybe String,
    publisher :: Maybe String,
    school :: Maybe String,
    series :: Maybe String,
    title :: Maybe String,
    reporttype :: Maybe String, -- 'type' in bibtex parlance
    volume :: Maybe String,
    year :: Maybe String
  }
  deriving (Eq, Read, Show)

fromFields :: Map Field.Field String -> [Author] -> BibtexType -> Key -> Entry
fromFields fields author bibtexType key =
  let
    abstract = lookup Abstract fields
    address = lookup Address fields
    annote = lookup Annote fields
    booktitle = lookup Booktitle fields
    chapter = lookup Chapter fields
    crossref = lookup Crossref fields
    doi = lookup DOI fields
    edition = lookup Edition fields
    editor = lookup Editor fields
    howpublished = lookup Howpublished fields
    institution = lookup Institution fields
    issn = lookup ISSN fields
    issue = lookup Issue fields
    journal = lookup Journal fields
    keywords = lookup Keywords fields
    month = lookup Month fields
    note = lookup Note fields
    number = lookup Number fields
    organization = lookup Organization fields
    pages = lookup Pages fields
    publisher = lookup Publisher fields
    school = lookup School fields
    series = lookup Series fields
    title = lookup Title fields
    reporttype = lookup Type fields
    volume = lookup Volume fields
    year = lookup Year fields
  in Entry {key = key, bibtexType = bibtexType, ..} -- TODO this can be simplified to Entry {..}

empty :: Key -> BibtexType -> Entry
empty k bt = Entry
  { bibtexType = bt
  , key = k
  , abstract = Nothing
  , address = Nothing
  , annote = Nothing
  , author = []
  , booktitle = Nothing
  , chapter = Nothing
  , crossref = Nothing
  , doi = Nothing
  , edition = Nothing
  , editor = Nothing
  , howpublished = Nothing
  , institution = Nothing
  , issn = Nothing
  , issue = Nothing
  , journal = Nothing
  , keywords = Nothing
  , month = Nothing
  , note = Nothing
  , number = Nothing
  , organization = Nothing
  , pages = Nothing
  , publisher = Nothing
  , school = Nothing
  , series = Nothing
  , title = Nothing
  , reporttype = Nothing
  , volume = Nothing
  , year = Nothing
  }
