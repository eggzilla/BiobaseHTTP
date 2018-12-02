-- | Data structures for Entrez HTTP queries

module Biobase.Entrez.HTTPData where

-- | Input datastructure for EntrezHTTP. Program is the selected eutility (e.g efetch, esearch),
--   database is the selected Entrez database (nucleotide) and query the query string.
data EntrezHTTPQuery = EntrezHTTPQuery
  { program :: Maybe String
  , database :: Maybe String
  , query :: String
  }
  deriving (Show, Eq)

-- | Data structure for Entrez summary result
data EntrezSummary = EntrezSummary
  {  documentSummaries :: [EntrezDocSum]}
  deriving (Show, Eq)

data EntrezDocSum = EntrezDocSum
  {  summaryId :: String
  ,  summaryItems :: [SummaryItem]}
  deriving (Show, Eq)

data SummaryItem = SummaryItem
  { itemName :: String
  , itemType :: String
  , itemContent :: String}
  deriving (Show, Eq)

-- | Gene doc summary
data EntrezGeneSummary = EntrezGeneSummary
  {  geneSummaries :: [EntrezGeneDocSummary]}
  deriving (Show, Eq)

data EntrezGeneDocSummary = EntrezGeneDocSummary
  { geneId :: String
  , geneName :: String
  , geneStatus :: String
  , geneCurrentID :: String
  , geneGeneticSource :: String
  , geneOtherAliases :: String
  , geneGenomicInfo :: EntrezGenomicInfo}
  deriving (Show, Eq)

data EntrezGenomicInfo = EntrezGenomicInfo
  { chrAccVer :: String
  , chrStart :: Int
  , chrStop :: Int
  , exonCount :: Int
  }
  deriving (Show, Eq)

-- | Data structure for Entrez search result
data EntrezSearch = EntrezSearch
  { count :: Int
  , retMax :: Int
  , retStart :: Int
  , searchIds :: [Int]
  , translationStack :: [TranslationStack]
  , queryTranslation :: String}
  deriving (Show, Eq)

data TranslationStack = TranslationStack
  { termSets :: [TermSet]
  , operation :: String}
  deriving (Show, Eq)

data TermSet = TermSet
  { term :: String
  , field :: String
  , termCount :: Int
  , explode :: String}
  deriving (Show, Eq)
