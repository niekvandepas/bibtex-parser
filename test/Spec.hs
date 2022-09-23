import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Parse (parseBibtex)
import Entry
import BibtexType (BibtexType(..))

main :: IO ()
main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    tests

tests =
    [ parsesSingleEntry
    -- , parsesMultipleEntries
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
\  pages = {106-125},\n\
\  publisher = {Routledge},\n\
\  title = {School composition and multiple ethnic identities of migrant-origin adolescents in the Netherlands},\n\
\  volume = {44},\n\
\  year = {2021},\n\
\}"
    actual = parseBibtex entry
    expected = Right $  [ entry1 ]

-- parsesMultipleEntries :: TestTree
-- parsesMultipleEntries = testCase "Parses a single entry" $ assertEqual "" expected actual
--   where
--     actual = _
--     expected = _

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
          , keywords = Nothing
          , month = Nothing
          , note = Nothing
          , number = Nothing
          , organization = Nothing
          , school = Nothing
          , series = Nothing
          , reporttype = Nothing
          }
