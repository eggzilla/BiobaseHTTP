-- | Data structures for Entrez HTTP queries

module Bio.EntrezHTTPData where
import Bio.TaxonomyData (Rank)

data EntrezHTTPQuery = EntrezHTTPQuery 
  { program :: Maybe String
  , database :: Maybe String
  , query :: String 
  }
  deriving (Show, Eq)

-- | Data structure for Entrez taxonomy fetch result
data Taxon = Taxon
  {  taxonomyId :: Int
  ,  scientificName :: String
  ,  parentTaxonomyId :: Int
  ,  rank :: Rank
  ,  division :: String
  ,  geneticCode :: GeneticCode
  ,  mitoGeneticCode :: MitoGeneticCode
  ,  lineage :: String
  ,  lineageEx :: [LineageTaxon]
  ,  createDate :: String
  ,  updateDate :: String
  ,  pubDate :: String
  } deriving (Show, Eq)

data TaxonName = TaxonName
  {  classCDE :: String
  ,  dispName :: String
  } deriving (Show, Eq)

data GeneticCode = GeneticCode
  {  gcId :: Int
  ,  gcName :: String
  } deriving (Show, Eq)

data MitoGeneticCode = MitoGeneticCode
  {  mgcId :: Int
  ,  mgcName :: String
  }
  deriving (Show, Eq)

data LineageTaxon = LineageTaxon
  {  lineageTaxId :: Int
  ,  lineageScienticName :: String
  ,  lineageRank :: Rank}
  deriving (Show, Eq)

-- | Simplified data structure 
data SimpleTaxon = SimpleTaxon
  {  simpleTaxonTaxId :: Int
  ,  simpleTaxonScientificName :: String
  ,  simpleTaxonParentTaxId :: Int
  ,  simpleTaxonRank :: Rank
  } deriving (Show, Eq)

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
