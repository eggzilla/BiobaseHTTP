{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

-- | Interface for the NCBI Entrez REST webservice
module Bio.EntrezHTTP (module Bio.EntrezHTTPData,
                       EntrezHTTPQuery(..),
                       entrezHTTP,
                       retrieveElementsEntrez,
                       portionListElements,                     
                       readEntrezTaxonSet,
                       readEntrezSimpleTaxons,
                       readEntrezParentIds,
                       readEntrezSummaries,
                       readEntrezSearch,
                       retrieveGeneSymbolFasta
                      ) where

import Network.HTTP.Conduit    
import qualified Data.ByteString.Lazy.Char8 as L8    
import Text.XML.HXT.Core
import Network
import Data.Maybe
import Bio.EntrezHTTPData
import Bio.TaxonomyData --(Rank,SimpleTaxon)
import qualified Data.ByteString.Char8 as B
      
-- | Send query and parse return XML 
startSession :: String -> String -> String -> IO String
startSession program' database' query' = do
  requestXml <- withSocketsDo
      $ sendQuery program' database' query'
  let requestXMLString = L8.unpack requestXml
  return requestXMLString

-- | Send query and return response XML
sendQuery :: String -> String -> String -> IO L8.ByteString
sendQuery program' database' query' = simpleHttp ("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"++ program' ++ ".fcgi?" ++ "db=" ++ database' ++ "&" ++ query')         

-- |
entrezHTTP :: EntrezHTTPQuery -> IO String
entrezHTTP (EntrezHTTPQuery program' database' query') = do
  let defaultProgram = "summary"
  let defaultDatabase = "nucleotide"                  
  let selectedProgram = fromMaybe defaultProgram program'
  let selectedDatabase = fromMaybe defaultDatabase database'  
  startSession selectedProgram selectedDatabase query'

-- | Wrapper functions that ensures that only 20 queries are sent per request
retrieveElementsEntrez :: [a] -> ([a] -> IO b) -> IO [b]
retrieveElementsEntrez listElements retrievalfunction = do
  let splits = portionListElements listElements 20
  entrezOutput <- mapM retrievalfunction splits
  return entrezOutput

portionListElements :: [a] -> Int -> [[a]]
portionListElements listElements elementsPerSplit
  | not (null listElements) = filter (\e ->not (null e)) result
  | otherwise = []
  where (heads,xs) = splitAt elementsPerSplit listElements
        result = (heads:(portionListElements xs elementsPerSplit))


-- Parsing functions

-- | Read entrez fetch for taxonomy database into a simplyfied datatype 
-- Result of e.g: http://eutils.ncbi.nlm.nih.
readEntrezTaxonSet :: String -> [Taxon]
readEntrezTaxonSet input = runLA (xreadDoc >>> parseEntrezTaxonSet) input

parseEntrezTaxonSet :: ArrowXml a => a XmlTree Taxon
parseEntrezTaxonSet = atTag "TaxaSet" >>> getChildren >>>
  proc entrezTaxons -> do
  _taxons <- parseEntrezTaxon -< entrezTaxons
  returnA -< _taxons

parseEntrezTaxon :: ArrowXml a => a XmlTree Taxon
parseEntrezTaxon = (isElem >>> hasName "Taxon") >>> 
  proc entrezTaxon -> do
    _taxonomyId <- getChildren >>> (isElem >>> hasName  "TaxId") >>> getChildren >>> getText -< entrezTaxon
    _scientificName <- getChildren >>> (isElem >>> hasName "ScientificName") >>> getChildren >>> getText -< entrezTaxon
    _parentTaxonomyId <- getChildren >>> (isElem >>> hasName "ParentTaxId") >>> getChildren >>> getText -< entrezTaxon
    _rank <- getChildren >>> (isElem >>> hasName "Rank") >>> getChildren >>> getText -< entrezTaxon
    _divison <- getChildren >>> (isElem >>> hasName "Division") >>> getChildren >>> getText -< entrezTaxon
    _geneticCode <- parseTaxonGeneticCode  -< entrezTaxon
    _mitoGeneticCode  <- parseTaxonMitoGeneticCode -< entrezTaxon
    _lineage <- getChildren >>> atTag "Lineage" >>> getChildren >>> getText -< entrezTaxon
    _lineageEx <- parseTaxonLineageEx -< entrezTaxon
    _createDate <- getChildren >>> (isElem >>> hasName "CreateDate") >>> getChildren >>> getText -< entrezTaxon
    _updateDate <- getChildren >>> (isElem >>> hasName "UpdateDate") >>> getChildren >>> getText -< entrezTaxon
    _pubDate <- getChildren >>> (isElem >>> hasName "PubDate") >>> getChildren >>> getText -< entrezTaxon
    returnA -< Taxon {
      taxonTaxId = read _taxonomyId :: Int,
      taxonScientificName = _scientificName,
      taxonParentTaxId = read _parentTaxonomyId :: Int,
      taxonRank = read _rank :: Rank,
      division = _divison,
      geneticCode = _geneticCode,
      mitoGeneticCode = _mitoGeneticCode,
      lineage = _lineage,
      lineageEx = _lineageEx,
      createDate = _createDate,
      updateDate = _updateDate,
      pubDate = _pubDate
    }
  
parseTaxonGeneticCode :: ArrowXml a => a XmlTree TaxGenCode
parseTaxonGeneticCode = getChildren >>> atTag "GeneticCode" >>>
  proc geneticcode -> do
  _gcId <- atTag "GCId" >>> getChildren >>> getText -< geneticcode
  _gcName <- atTag "GCName" >>> getChildren >>> getText -< geneticcode
  returnA -< TaxGenCode {
    geneticCodeId = read _gcId :: Int,
    abbreviation = Nothing,
    geneCodeName = _gcName,
    cde = [],
    starts = []      
    }

parseTaxonMitoGeneticCode :: ArrowXml a => a XmlTree TaxGenCode
parseTaxonMitoGeneticCode = getChildren >>> atTag "MitoGeneticCode" >>>
  proc mitogeneticcode -> do
  _mgcId <- atTag "MGCId" >>> getChildren >>> getText -< mitogeneticcode
  _mgcName <- atTag "MGCName" >>> getChildren >>> getText -< mitogeneticcode
  returnA -< TaxGenCode {
    geneticCodeId = read _mgcId :: Int,
    abbreviation = Nothing,
    geneCodeName = _mgcName,
    cde = [],
    starts = []
    }

parseTaxonLineageEx :: ArrowXml a => a XmlTree [LineageTaxon]
parseTaxonLineageEx = getChildren >>> atTag "LineageEx" >>>
  proc taxonLineageEx -> do
  _lineageEx <- listA parseLineageTaxon -< taxonLineageEx
  returnA -< _lineageEx

parseLineageTaxon :: ArrowXml a => a XmlTree LineageTaxon
parseLineageTaxon = getChildren >>> atTag "Taxon" >>>
  proc lineageTaxon -> do
  _lineageTaxId <- atTag "TaxId" >>> getChildren >>> getText -< lineageTaxon
  _lineageScienticName <- atTag "ScientificName" >>> getChildren >>> getText -< lineageTaxon
  _lineageRank <- atTag "Rank" >>> getChildren >>> getText -< lineageTaxon
  returnA -< LineageTaxon {
    lineageTaxId = read _lineageTaxId :: Int, 
    lineageScienticName = _lineageScienticName,
    lineageRank = read _lineageRank :: Rank
    }

-- | Read entrez fetch for taxonomy database into a simplyfied datatype 
-- Result of e.g: http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=taxonomy&id=1406860
readEntrezSimpleTaxons :: String -> [SimpleTaxon]
readEntrezSimpleTaxons input = runLA (xreadDoc >>> parseEntrezSimpleTaxons) input

parseEntrezSimpleTaxons :: ArrowXml a => a XmlTree SimpleTaxon
parseEntrezSimpleTaxons = getChildren >>> atTag "Taxon" >>>
  proc entrezSimpleTaxon -> do
  simple_TaxId <- atTag "TaxId" >>> getChildren >>> getText -< entrezSimpleTaxon
  simple_ScientificName <- atTag "ScientificName" >>> getChildren >>> getText -< entrezSimpleTaxon
  simple_ParentTaxId <- atTag "ParentTaxId" >>> getChildren >>> getText -< entrezSimpleTaxon
  simple_Rank <- atTag "Rank" >>> getChildren >>> getText -< entrezSimpleTaxon
  returnA -< SimpleTaxon {
    simpleTaxId = read simple_TaxId :: Int,
    simpleScientificName = (B.pack simple_ScientificName),
    simpleParentTaxId = read simple_ParentTaxId :: Int,
    simpleRank = read simple_Rank :: Rank
    } 

readEntrezParentIds :: String -> [Int]
readEntrezParentIds input = runLA (xreadDoc >>> parseEntrezParentTaxIds) input

parseEntrezParentTaxIds :: ArrowXml a => a XmlTree Int
parseEntrezParentTaxIds = getChildren >>> atTag "Taxon" >>>
  proc entrezSimpleTaxon -> do
  simple_ParentTaxId <- atTag "ParentTaxId" >>> getChildren >>> getText -< entrezSimpleTaxon
  returnA -< read simple_ParentTaxId :: Int
    
-- | Read entrez summary from internal haskell string
readEntrezSummaries :: String -> [EntrezSummary]
readEntrezSummaries input = runLA (xreadDoc >>> parseEntrezSummaries) input

-- | Parse entrez summary result
parseEntrezSummaries :: ArrowXml a => a XmlTree EntrezSummary
parseEntrezSummaries = atTag "eSummaryResult" >>> 
  proc entrezSummary -> do
  document_Summaries <- listA parseEntrezDocSums -< entrezSummary
  returnA -< EntrezSummary {
    documentSummaries = document_Summaries
    }     

-- | 
parseEntrezDocSums :: ArrowXml a => a XmlTree EntrezDocSum
parseEntrezDocSums = atTag "DocSum" >>> 
  proc entrezDocSum -> do
  summary_Id <- atTag "Id" >>> getChildren >>> getText -< entrezDocSum
  summary_Items <- listA parseSummaryItems -< entrezDocSum
  returnA -< EntrezDocSum {
    summaryId = summary_Id,
    summaryItems = summary_Items
    } 

-- | 
parseSummaryItems :: ArrowXml a => a XmlTree SummaryItem
parseSummaryItems = atTag "Item" >>> 
  proc summaryItem -> do
  item_Name <- getAttrValue "Name" -< summaryItem
  item_Type <- getAttrValue "Type" -< summaryItem
  item_Content <- getText <<< getChildren -< summaryItem
  returnA -< SummaryItem {
    itemName = item_Name,
    itemType = item_Type,
    itemContent = item_Content
    } 

-- | Read entrez summary from internal haskell string
readEntrezSearch :: String -> [EntrezSearch]
readEntrezSearch input = runLA (xreadDoc >>> parseEntrezSearch) input

-- | Parse entrez search result
parseEntrezSearch :: ArrowXml a => a XmlTree EntrezSearch
parseEntrezSearch = atTag "eSearchResult" >>> 
  proc entrezSearch -> do
  _count <- atTag "Count" >>> getChildren >>> getText -< entrezSearch
  _retMax <- atTag "RetMax" >>> getChildren >>> getText -< entrezSearch
  _retStart <- atTag "RetStart" >>> getChildren >>> getText -< entrezSearch
  _searchIds <- atTag "IdList" >>> listA parseSearchId -< entrezSearch
  _translationStack <- listA parseTranslationStack -< entrezSearch
  _queryTranslation <- atTag "QueryTranslation" >>> getChildren >>> getText -< entrezSearch
  returnA -< EntrezSearch {
    count = (readInt _count),
    retMax = (readInt _retMax),
    retStart = (readInt _retStart),
    searchIds = _searchIds,
    translationStack = _translationStack,
    queryTranslation = _queryTranslation
    }     

-- | Parse entrez TranslationStack
parseSearchIds :: ArrowXml a => a XmlTree [Int]
parseSearchIds = atTag "IdList" >>> 
  proc entrezSearchIds -> do
  _searchIds <- listA parseSearchId -< entrezSearchIds
  returnA -< _searchIds

-- | Parse entrez TranslationStack
parseSearchId :: ArrowXml a => a XmlTree Int
parseSearchId = atTag "Id" >>> 
  proc entrezSearchId -> do
  searchId <- getChildren >>> getText -< entrezSearchId
  returnA -< (readInt searchId)

-- | Parse entrez TranslationStack
parseTranslationStack :: ArrowXml a => a XmlTree TranslationStack
parseTranslationStack = atTag "TranslationStack" >>> 
  proc entrezTranslationStack -> do
  _termSets <- listA parseTermSet -< entrezTranslationStack
  _operation <- atTag "OP" >>> getChildren >>> getText -< entrezTranslationStack
  returnA -< TranslationStack {
    termSets = _termSets,
    operation = _operation
    } 

-- | Parse entrez TermSet 
parseTermSet :: ArrowXml a => a XmlTree TermSet
parseTermSet = atTag "TermSet" >>> 
  proc entrezTermSet -> do
  _term <- atTag "Term" >>> getChildren >>> getText -< entrezTermSet
  _field <- atTag "Field" >>> getChildren >>> getText -< entrezTermSet
  _termCount <- atTag "Count" >>> getChildren >>> getText -< entrezTermSet
  _explode <- atTag "Explode" >>> getChildren >>> getText -< entrezTermSet
  returnA -< TermSet {
    term = _term,
    field = _field,
    termCount = readInt _termCount,
    explode = _explode
    } 

-- | gets all subtrees with the specified tag name
atTag :: ArrowXml a =>  String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

-- retrieval functions

-- | Retrieve sequence for gene symbol (e.g. yhfA) from accession number (e.g. NC_000913.3)
retrieveGeneSymbolFasta :: String -> String -> IO Int
retrieveGeneSymbolFasta genesymbol accession = do
  let query1 = EntrezHTTPQuery (Just "esearch") (Just "gene") ("term=%28" ++ genesymbol ++ "[Gene%20Name]%29%20AND%20" ++ accession ++ "[Nucleotide%20Accession]")
  uniqueidresponse <- entrezHTTP query1
  let uniqueid = head (searchIds (head (readEntrezSearch uniqueidresponse)))
  return uniqueid

readInt :: String -> Int
readInt = read
