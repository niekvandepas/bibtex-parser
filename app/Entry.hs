{-# LANGUAGE RecordWildCards #-}
module Entry (Entry(..), fromFields) where
import BibtexType (BibtexType)
import Data.Map (Map, lookup)
import Field (Field (..))
import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok

type Key = String

data Entry = Entry
  { bibtexType :: BibtexType,
    key :: Key,
    abstract :: Maybe String,
    address :: Maybe String,
    annote :: Maybe String,
    author :: Maybe String,
    booktitle :: Maybe String,
    chapter :: Maybe String,
    crossref :: Maybe String,
    doi :: Maybe String,
    edition :: Maybe String,
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

instance FromRow Entry where
  fromRow =
    Entry <$>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field

instance ToRow Entry where
  toRow
    (Entry pBibtexType pKey pAbstract pAddress pAnnote pAuthor pBooktitle pChapter pCrossref pDoi pEdition pEditor pHowpublished pInstitution pIssn pIssue pJournal pKeywords pMonth pNote pNumber pOrganization pPages pPublisher pSchool pSeries pTitle pReporttype pVolume pYear)
    -- = toRow (pBibtexType, pKey, pAbstract, pAddress, pAnnote, pAuthor, pBooktitle, pChapter, pCrossref, pDoi, pEdition, pEditor, pHowpublished, pInstitution, pIssn, pIssue, pJournal, pKeywords, pMonth, pNote, pNumber, pOrganization, pPages, pPublisher, pSchool, pSeries, pTitle, pReporttype, pVolume, pYear)
    = undefined

fromFields :: Map Field.Field String -> BibtexType -> Key -> Entry
fromFields fields bibtexType key =
  let
    abstract = lookup Abstract fields
    address = lookup Address fields
    annote = lookup Annote fields
    author = lookup Author fields
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
