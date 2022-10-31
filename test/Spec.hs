import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Parse.Bibtex (parseBibtex)
import Parse.Search.Internal (parseSearch, search)
import Entry
import BibtexType (BibtexType(..))
import Field (Field (..))
import Data.Map (Map, fromList)
import Entry (empty)
import UI (ui)
import Data.Either (isLeft)
import Entry (ValidationPredicate(..))

main :: IO ()
main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    tests

tests =
    [ parsesSingleEntry
    , parsesMultipleEntries
    , failsOnInvalidBibtexType
    , parsesSearchQuery
    , parsesComplexSearchQuery
    , parsesShorthandSearchQuery
    , performsSearch
    , performsSearchWithNoResult
    , failsOnInvalidField
    , validatesValidEntry
    , validatesInvalidEntry
    ]

parsesSingleEntry :: TestTree
parsesSingleEntry = testCase "Parses a single entry" $ assertEqual "" expected actual
  where
    entry = "@article{Veerman2021,\n\
\  abstract = {Ethnic identity is central to many contemporary discussions of belonging and assimilation of migrant-origin youth. Studies typically focus on a single minority identity. Identity theory implies, however, that individuals may hold multiple ethnic identities, or none, and these may find expression to a greater or less extent depending on context. Using a nationally representative, longitudinal study of Dutch teenagers, we investigate the role of classroom ethnic composition in shaping multiple ethnic identity expression. Framing identity choices as a relational process, we show that the number of ethnic identities that children with a migrant-origin background choose is greater for those students who are exposed to a more ethnically diverse context, while less diverse classrooms foster ethnic identification with no or fewer minority groups. Classification of migrant-origin students with a single (minority) ethnicity may thus be an oversimplification of ethnic identity, even for those from a single country of origin.},\n\
\  author = {Gert Jan Veerman and Lucinda Platt},\n\
\  doi = {10.1080/01419870.2021.1887503},\n\
\  issn = {14664356},\n\
\  issue = {16},\n\
\  journal = {Ethnic and Racial Studies},\n\
\  keywords = {Ethnic identity,adolescents,classroom composition,ethnic diversity,migrant-origin background,multiple identity},\n\
\  pages = {106-125},\n\
\  publisher = {Routledge},\n\
\  title = {School composition and multiple ethnic identities of migrant-origin adolescents in the Netherlands},\n\
\  volume = {44},\n\
\  year = {2021}\n\
\}"
    actual = parseBibtex entry
    expected = Right $  [ entry1 ]

parsesMultipleEntries :: TestTree
parsesMultipleEntries = testCase "Parses multiple entries" $ assertEqual "" expected actual
  where
    actual = parseBibtex bibtex
    expected = Right [entry1, entry2]
    bibtex =
      "@article{Veerman2021,\n\
      \   abstract = {Ethnic identity is central to many contemporary discussions of belonging and assimilation of migrant-origin youth. Studies typically focus on a single minority identity. Identity theory implies, however, that individuals may hold multiple ethnic identities, or none, and these may find expression to a greater or less extent depending on context. Using a nationally representative, longitudinal study of Dutch teenagers, we investigate the role of classroom ethnic composition in shaping multiple ethnic identity expression. Framing identity choices as a relational process, we show that the number of ethnic identities that children with a migrant-origin background choose is greater for those students who are exposed to a more ethnically diverse context, while less diverse classrooms foster ethnic identification with no or fewer minority groups. Classification of migrant-origin students with a single (minority) ethnicity may thus be an oversimplification of ethnic identity, even for those from a single country of origin.},\n\
      \   author = {Gert Jan Veerman and Lucinda Platt},\n\
      \   doi = {10.1080/01419870.2021.1887503},\n\
      \   issn = {14664356},\n\
      \   issue = {16},\n\
      \   journal = {Ethnic and Racial Studies},\n\
      \   keywords = {Ethnic identity,adolescents,classroom composition,ethnic diversity,migrant-origin background,multiple identity},\n\
      \   pages = {106-125},\n\
      \   publisher = {Routledge},\n\
      \   title = {School composition and multiple ethnic identities of migrant-origin adolescents in the Netherlands},\n\
      \   volume = {44},\n\
      \   year = {2021}\n\
      \}\n\
      \@incollection{Schielke,\n\
      \  author    = {Samuli Schielke},\n\
      \  editor    = {Carla Freeman and Rachel Heiman and Mark Liechty},\n\
      \  title     = {Living in the Future Tense: Aspiring for world and class in provincial Egypt},\n\
      \  booktitle = {The Global Middle Classes: Theorizing through Ethnography},\n\
      \  pages     = {31-56},\n\
      \  publisher = {School for Advanced Research Press},\n\
      \  year      = {2012}\n\
      \}"

failsOnInvalidBibtexType = testCase "Refuses to parse an entry with an invalid bibtex type" $ assertBool "" condition
  where
    entry = "@orticle{Veerman2021,\n\
\  abstract = {Ethnic identity is central to many contemporary discussions of belonging and assimilation of migrant-origin youth. Studies typically focus on a single minority identity. Identity theory implies, however, that individuals may hold multiple ethnic identities, or none, and these may find expression to a greater or less extent depending on context. Using a nationally representative, longitudinal study of Dutch teenagers, we investigate the role of classroom ethnic composition in shaping multiple ethnic identity expression. Framing identity choices as a relational process, we show that the number of ethnic identities that children with a migrant-origin background choose is greater for those students who are exposed to a more ethnically diverse context, while less diverse classrooms foster ethnic identification with no or fewer minority groups. Classification of migrant-origin students with a single (minority) ethnicity may thus be an oversimplification of ethnic identity, even for those from a single country of origin.},\n\
\  author = {Gert Jan Veerman and Lucinda Platt},\n\
\  doi = {10.1080/01419870.2021.1887503},\n\
\  issn = {14664356},\n\
\  issue = {16},\n\
\  journal = {Ethnic and Racial Studies},\n\
\  pages = {106-125},\n\
\  publisher = {Routledge},\n\
\  title = {School composition and multiple ethnic identities of migrant-origin adolescents in the Netherlands},\n\
\  volume = {44},\n\
\  year = {2021},\n\
\}"
    condition = isLeft $ parseBibtex entry

failsOnInvalidField = testCase "Refuses to parse an entry with an invalid field" $ assertBool "" condition
  where
    entry = "@article{Veerman2021,\n\
\  abstract = {Ethnic identity is central to many contemporary discussions of belonging and assimilation of migrant-origin youth. Studies typically focus on a single minority identity. Identity theory implies, however, that individuals may hold multiple ethnic identities, or none, and these may find expression to a greater or less extent depending on context. Using a nationally representative, longitudinal study of Dutch teenagers, we investigate the role of classroom ethnic composition in shaping multiple ethnic identity expression. Framing identity choices as a relational process, we show that the number of ethnic identities that children with a migrant-origin background choose is greater for those students who are exposed to a more ethnically diverse context, while less diverse classrooms foster ethnic identification with no or fewer minority groups. Classification of migrant-origin students with a single (minority) ethnicity may thus be an oversimplification of ethnic identity, even for those from a single country of origin.},\n\
\  author = {Gert Jan Veerman and Lucinda Platt},\n\
\  doi = {10.1080/01419870.2021.1887503},\n\
\  issn = {14664356},\n\
\  issue = {16},\n\
\  journal = {Ethnic and Racial Studies},\n\
\  pages = {106-125},\n\
\  publisher = {Routledge},\n\
\  title = {School composition and multiple ethnic identities of migrant-origin adolescents in the Netherlands},\n\
\  volume = {44},\n\
\  yeer = {2021},\n\
\}"
    condition = isLeft $ parseBibtex entry

parsesSearchQuery = testCase "Parses a simple search query" $ assertEqual "" expected actual
  where
    expected :: Map Field (Maybe String)
    expected = fromList [(Author, Just "someone")]
    actual = parseSearch "author = someone"

parsesComplexSearchQuery = testCase "Parses a complex search query" $ assertEqual "" expected actual
  where
    expected :: Map Field (Maybe String)
    expected = fromList [(Author, Just "someone"), (Title, Just "Hello world!"), (Year, Just "2022")]
    actual = parseSearch "author = someone; title = Hello world!; year=2022"

parsesShorthandSearchQuery = testCase "Parses a complex search query" $ assertEqual "" expected actual
  where
    expected :: Map Field (Maybe String)
    expected = fromList [(Author, Just "someone"), (Title, Just "Hello world!"), (Year, Just "2022")]
    actual = parseSearch "au = someone; t = Hello world!; y=2022"

performsSearch = testCase "Performs a simple search query" $ assertEqual "" expected actual
  where
    expected = [entry1]
    actual = search "author = Gert Jan Veerman" [entry1, empty "" Article]

performsSearchWithNoResult = testCase "Performs a search query with no results" $ assertEqual "" expected actual
  where
    expected = []
    actual = search "author = gork bork" [entry1, empty "" Article]

validatesValidEntry = testCase "Validates an entry" $ assertEqual "" expected actual
  where
    expected = []
    actual = validate entry1

validatesInvalidEntry = testCase "Validates an invalid entry" $ assertEqual "" expected actual
  where
    expected = [Contains Author, Contains Title, Contains Journal]
    actual = validate invalidEntry

entry1 = Entry
          { bibtexType = Article
          , key = "Veerman2021"
          , abstract = Just "Ethnic identity is central to many contemporary discussions of belonging and assimilation of migrant-origin youth. Studies typically focus on a single minority identity. Identity theory implies, however, that individuals may hold multiple ethnic identities, or none, and these may find expression to a greater or less extent depending on context. Using a nationally representative, longitudinal study of Dutch teenagers, we investigate the role of classroom ethnic composition in shaping multiple ethnic identity expression. Framing identity choices as a relational process, we show that the number of ethnic identities that children with a migrant-origin background choose is greater for those students who are exposed to a more ethnically diverse context, while less diverse classrooms foster ethnic identification with no or fewer minority groups. Classification of migrant-origin students with a single (minority) ethnicity may thus be an oversimplification of ethnic identity, even for those from a single country of origin."
          , author = ["Gert Jan Veerman",  "Lucinda Platt"]
          , doi = Just "10.1080/01419870.2021.1887503"
          , issn = Just "14664356"
          , issue = Just "16"
          , journal = Just "Ethnic and Racial Studies"
          , pages = Just "106-125"
          , publisher = Just "Routledge"
          , title = Just "School composition and multiple ethnic identities of migrant-origin adolescents in the Netherlands"
          , volume = Just "44"
          , year = Just "2021"
          , address = Nothing
          , annote = Nothing
          , booktitle = Nothing
          , chapter = Nothing
          , crossref = Nothing
          , edition = Nothing
          , editor = Nothing
          , howpublished = Nothing
          , institution = Nothing
          , keywords = Just "Ethnic identity,adolescents,classroom composition,ethnic diversity,migrant-origin background,multiple identity"
          , month = Nothing
          , note = Nothing
          , number = Nothing
          , organization = Nothing
          , school = Nothing
          , series = Nothing
          , reporttype = Nothing
          }

entry2 :: Entry
entry2 = Entry
          { bibtexType = Incollection
          , key = "Schielke"
          , abstract = Nothing
          , author = ["Samuli Schielke"]
          , doi = Nothing
          , issn = Nothing
          , issue = Nothing
          , journal = Nothing
          , pages = Just "31-56"
          , publisher = Just "School for Advanced Research Press"
          , title = Just "Living in the Future Tense: Aspiring for world and class in provincial Egypt"
          , volume = Nothing
          , year = Just "2012"
          , address = Nothing
          , annote = Nothing
          , booktitle = Just "The Global Middle Classes: Theorizing through Ethnography"
          , chapter = Nothing
          , crossref = Nothing
          , edition = Nothing
          , editor = Just "Carla Freeman and Rachel Heiman and Mark Liechty"
          , howpublished = Nothing
          , institution = Nothing
          , keywords = Nothing
          , month = Nothing
          , note = Nothing
          , number = Nothing
          , organization = Nothing
          , school = Nothing
          , series = Nothing
          , reporttype = Nothing
          }

invalidEntry = (empty "invalidEntry" Article) {year = Just "2012"}
