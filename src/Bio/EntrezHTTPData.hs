-- | Data structures for Entrez HTTP queries

module Bio.EntrezHTTPData where

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
