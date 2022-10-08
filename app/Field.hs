module Field where
import Data.List (isPrefixOf)
import Data.Char (toLower)

data Field
  = Abstract
  | Address
  | Annote
  | Author
  | Booktitle
  | Chapter
  | Crossref
  | DOI
  | Edition
  | Editor
  | Howpublished
  | Institution
  | ISSN
  | Issue
  | Journal
  | Keywords
  | Month
  | Note
  | Number
  | Organization
  | Pages
  | Publisher
  | School
  | Series
  | Title
  | Type
  | Volume
  | Year
  deriving (Show, Eq, Ord)

instance Read Field where
  readsPrec _ s
    | "abstract" `isPrefixOf` s' = [(Abstract, drop (length "abstract") s)]
    | "annote" `isPrefixOf` s' = [(Annote, drop (length "annote") s)]
    -- Address
    | "addr" `isPrefixOf` s' = [(Address, drop (length "address") s)]
    -- Author
    | "au" `isPrefixOf` s' = [(Author, drop (length "author") s)]
    | "booktitle" `isPrefixOf` s' = [(Booktitle, drop (length "booktitle") s)]
    | "ch" `isPrefixOf` s' = [(Chapter, drop (length "chapter") s)]
    | "chapter" `isPrefixOf` s' = [(Chapter, drop (length "chapter") s)]
    | "crossref" `isPrefixOf` s' = [(Crossref, drop (length "crossref") s)]
    | "doi" `isPrefixOf` s' = [(DOI, drop (length "doi") s)]
    | "edition" `isPrefixOf` s' = [(Edition, drop (length "edition") s)]
    | "editor" `isPrefixOf` s' = [(Editor, drop (length "editor") s)]
    | "ed" == s' = [(Editor, drop (length "editor") s)]
    | "howpublished" `isPrefixOf` s' = [(Howpublished, drop (length "howpublished") s)]
    | "institution" `isPrefixOf` s' = [(Institution, drop (length "institution") s)]
    | "issn" `isPrefixOf` s' = [(ISSN, drop (length "issn") s)]
    | "issue" `isPrefixOf` s' = [(Issue, drop (length "issue") s)]
    | "iss" == s' = [(Issue, drop (length "issue") s)]
    | "jour" `isPrefixOf` s' = [(Journal, drop (length "journal") s)]
    -- Keywords
    | "keyw" `isPrefixOf` s'= [(Keywords, drop (length "keywords") s)]
    | "kw" == s'= [(Keywords, drop (length "keywords") s)]
    -- Month
    | "mo" `isPrefixOf` s' = [(Month, drop (length "month") s)]
    | "note" `isPrefixOf` s'= [(Note, drop (length "note") s)]
    | "number" `isPrefixOf` s' = [(Number, drop (length "number") s)]
    | "no" == s' = [(Number, drop (length "number") s)]
    -- Organization
    | "org" `isPrefixOf` s' = [(Organization, drop (length "organization") s)]
    -- Pages
    | "pag" `isPrefixOf` s' = [(Pages, drop (length "pages") s)]
    | "pp" == s' = [(Pages, drop (length "pages") s)]
    -- Publisher
    | "pub" `isPrefixOf` s' = [(Publisher, drop (length "publisher") s)]
    -- School
    | "sch" `isPrefixOf` s' = [(School, drop (length "school") s)]
    | "series" `isPrefixOf` s' = [(Series, drop (length "series") s)]
    | "title" `isPrefixOf` s' = [(Title, drop (length "title") s)]
    | "t" == s' = [(Title, drop (length "title") s)]
    | "type" `isPrefixOf` s'= [(Type, drop (length "type") s)]
    -- Volume
    | "vol" `isPrefixOf` s' = [(Volume, drop (length "volume") s)]
    | "year" `isPrefixOf` s'= [(Year, drop (length "year") s)]
    | "y" == s'= [(Year, drop (length "year") s)]
    | otherwise = error $ "read Field: no parse for '" ++ s ++ "'"
      where s' = map toLower s
