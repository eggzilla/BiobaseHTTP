-- | 

module Bio.EntrezHTTPData where

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
  , resultIds :: [Int]}
  deriving (Show, Eq)
