{-# LANGUAGE RecordWildCards #-}
module Entry (Entry(..), fromFields, empty, validate, ValidationPredicate(..)) where
import BibtexType (BibtexType (..))
import Data.Map (Map, lookup)
import Field (Field (..))
import Prelude hiding (lookup)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes, isJust)
import Data.List ((\\), intercalate)
import Debug.Trace (trace)

type Key = String
type Author = String

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

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}

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

{--------------------------------------------------------------------
  Validation
--------------------------------------------------------------------}

data ValidationPredicate  = Contains Field
                          | ContainsOneOf [Field]
                          deriving (Eq)

instance Show ValidationPredicate where
  show (Contains field) = "Should contain the following: " ++ show field ++ "."
  show (ContainsOneOf fields) = "Should contain one of the following: " ++ intercalate ", " (map show fields) ++ "."

validate :: Entry -> [ValidationPredicate]
validate entry = validate' (bibtexType entry) entry
  where
    validate' :: BibtexType -> Entry -> [ValidationPredicate]
    validate' bt e = mapMaybe (runPred e) (preds bt)

runPred :: Entry -> ValidationPredicate -> Maybe ValidationPredicate
runPred e (Contains field) = if fieldHasValue e field then Nothing else Just $ Contains field
runPred e (ContainsOneOf fields) = if all ((== True) . fieldHasValue e) fields then Nothing else Just $ ContainsOneOf fields

preds :: BibtexType -> [ValidationPredicate]
preds Article = [Contains Author, Contains Title, Contains Journal, Contains Year]
preds Book = [ContainsOneOf [Author, Editor], Contains Title, Contains Publisher, Contains Year]
preds Booklet = [Contains Title]
preds Inbook = [Contains Author, Contains Editor, Contains Title, Contains Publisher, Contains Year]
preds Incollection = [Contains Author, Contains Title, Contains Booktitle, Contains Publisher, Contains Year]
preds Inproceedings = [Contains Author, Contains Title, Contains Booktitle, Contains Year]
preds Manual = [Contains Title]
preds Mastersthesis = [Contains Author, Contains Title, Contains School, Contains Year]
preds Misc = []
preds Phdthesis = [Contains Author, Contains Title, Contains School, Contains Year]
preds Proceedings = [Contains Title, Contains Year]
preds Techreport = [Contains Author, Contains Title, Contains Institution, Contains Year]
preds Unpublished = [Contains Author, Contains Title, Contains Note]

fieldHasValue :: Entry -> Field -> Bool
fieldHasValue e Abstract = isJust (abstract e)
fieldHasValue e Address = isJust (address e)
fieldHasValue e Annote = isJust (annote e)
fieldHasValue e Author = (not . null) (author e)
fieldHasValue e Booktitle = isJust (booktitle e)
fieldHasValue e Chapter = isJust (chapter e)
fieldHasValue e Crossref = isJust (crossref e)
fieldHasValue e DOI = isJust (doi e)
fieldHasValue e Edition = isJust (edition e)
fieldHasValue e Editor = isJust (editor e)
fieldHasValue e Howpublished = isJust (howpublished e)
fieldHasValue e Institution = isJust (institution e)
fieldHasValue e ISSN = isJust (issn e)
fieldHasValue e Issue = isJust (issue e)
fieldHasValue e Journal = isJust (journal e)
fieldHasValue e Keywords = isJust (keywords e)
fieldHasValue e Month = isJust (month e)
fieldHasValue e Note = isJust (note e)
fieldHasValue e Number = isJust (number e)
fieldHasValue e Organization = isJust (organization e)
fieldHasValue e Pages = isJust (pages e)
fieldHasValue e Publisher = isJust (publisher e)
fieldHasValue e School = isJust (school e)
fieldHasValue e Series = isJust (series e)
fieldHasValue e Title = isJust (title e)
fieldHasValue e Volume = isJust (volume e)
fieldHasValue e Year = isJust (year e)
