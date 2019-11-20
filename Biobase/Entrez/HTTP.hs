{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

-- | Interface for the NCBI Entrez REST webservice.
--
--   The entrezHTTP function provides a interface to the NCBI Entrez REST service.
--
--   A series of different eutilites and databases are provided by the REST interface.
--   Response depends on the combination of eutil and database, as well requested returntype.
--   Specific combinations have wrapper functions with corresponding parsing functions included (see Usage example).
--
--   If you use this libary in a tool, please read <http://www.ncbi.nlm.nih.gov/books/NBK25497/ A General Introduction to the E-utilities> carefully and register your tool at eutilities@ncbi.nlm.nih.gov. You can append your registration info generated
--   with the included buildRegistration function to your query.
--
-- == Usage example:
--
-- Retrieve a nucleotide sequence for Escherichia coli
--
-- >  nucleotideFasta <- fetchNucleotideString "NC_000913.3" 50 1000 "+" Nothing

module Biobase.Entrez.HTTP (-- * Datatypes
                       module Biobase.Entrez.HTTPData,
                       -- * Retrieval function
                       entrezHTTP,
                       retrieveGeneSymbolFasta,
                       fetchNucleotideString,
                       retrieveElementsEntrez,
                       portionListElements,
                       -- * Parsing functions
                       readEntrezTaxonSet,
                       readEntrezSimpleTaxons,
                       readEntrezParentIds,
                       readEntrezSummaries,
                       readEntrezSearch,
                       -- * auxiliary functions
                       buildRegistration,
                       maybeBuildRegistration,
                       setStrand,
                       convertCoordinatesToStrand
                      ) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B
import Text.XML.HXT.Core
import Network.Socket
import Data.Maybe
import Biobase.Entrez.HTTPData
import Biobase.Taxonomy.Types
import Network.HTTP.Base
import qualified Data.Text.Lazy as TL

-- | Send query and parse return XML
startSession :: String -> String -> String -> IO String
startSession program' database' query' = do
  requestXml <- withSocketsDo
      $ sendQuery program' database' query'
  let requestXMLString = L8.unpack requestXml
  return requestXMLString

-- | Send query and return response XML
sendQuery :: String -> String -> String -> IO L8.ByteString
sendQuery program' database' query' = simpleHttp ("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"++ program' ++ ".fcgi?" ++ "db=" ++ database' ++ "&" ++ query')

-- | Function for querying the NCBI entrez REST interface. Input EntrezHTTPQuery datatype is used to select database, program of interest and contains the query string.
--   Please note that query strings containing whitespace or special characters need to be urlencoded. The response format and content depends on the query type, the output
-- therefore provided as String.
entrezHTTP :: EntrezHTTPQuery -> IO String
entrezHTTP (EntrezHTTPQuery program' database' query') = do
  let defaultProgram = "summary"
  let defaultDatabase = "nucleotide"
  let selectedProgram = fromMaybe defaultProgram program'
  let selectedDatabase = fromMaybe defaultDatabase database'
  startSession selectedProgram selectedDatabase query'

-- | Wrapper function for eutils that accept a list of querys (e.g. a list of gene ids) that ensures that only chunks of 20 queries are sent per request. Sending to long queries otherwise results in a serverside exception.
retrieveElementsEntrez :: [a] -> ([a] -> IO b) -> IO [b]
retrieveElementsEntrez listElements retrievalfunction = do
  let splits = portionListElements listElements 20
  mapM retrievalfunction splits

-- Auxiliary function for retrieveElementsEntrez
portionListElements :: [a] -> Int -> [[a]]
portionListElements listElements elementsPerSplit
  | not (null listElements) = filter (not . null) result
  | otherwise = []
  where (heads,xs) = splitAt elementsPerSplit listElements
        result = heads:portionListElements xs elementsPerSplit

---------------------------------------
-- Parsing functions

-- | Read entrez fetch for taxonomy database into a simplyfied datatype
-- Result of e.g: http://eutils.ncbi.nlm.nih.
readEntrezTaxonSet :: String -> [Taxon]
readEntrezTaxonSet = runLA (xreadDoc >>> parseEntrezTaxonSet)

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
      taxonScientificName = (B.pack _scientificName),
      taxonParentTaxId = read _parentTaxonomyId :: Int,
      taxonRank = read _rank :: Rank,
      division = (B.pack _divison),
      geneticCode = _geneticCode,
      mitoGeneticCode = _mitoGeneticCode,
      lineage = (B.pack _lineage),
      lineageEx = _lineageEx,
      createDate = (B.pack _createDate),
      updateDate = (B.pack _updateDate),
      pubDate = (B.pack _pubDate)
    }

parseTaxonGeneticCode :: ArrowXml a => a XmlTree TaxGenCode
parseTaxonGeneticCode = getChildren >>> atTag "GeneticCode" >>>
  proc geneticcode -> do
  _gcId <- atTag "GCId" >>> getChildren >>> getText -< geneticcode
  _gcName <- atTag "GCName" >>> getChildren >>> getText -< geneticcode
  returnA -< TaxGenCode {
    geneticCodeId = read _gcId :: Int,
    abbreviation = B.empty,
    geneCodeName = (B.pack _gcName),
    cde = B.empty,
    starts = B.empty
    }

parseTaxonMitoGeneticCode :: ArrowXml a => a XmlTree TaxGenCode
parseTaxonMitoGeneticCode = getChildren >>> atTag "MitoGeneticCode" >>>
  proc mitogeneticcode -> do
  _mgcId <- atTag "MGCId" >>> getChildren >>> getText -< mitogeneticcode
  _mgcName <- atTag "MGCName" >>> getChildren >>> getText -< mitogeneticcode
  returnA -< TaxGenCode {
    geneticCodeId = read _mgcId :: Int,
    abbreviation = B.empty,
    geneCodeName = B.pack _mgcName,
    cde = B.empty,
    starts = B.empty
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
    lineageScienticName = (B.pack _lineageScienticName),
    lineageRank = read _lineageRank :: Rank
    }

-- | Read entrez fetch for taxonomy database into a simplyfied datatype
-- Result of e.g: http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=taxonomy&id=1406860
readEntrezSimpleTaxons :: String -> [SimpleTaxon]
readEntrezSimpleTaxons = runLA (xreadDoc >>> parseEntrezSimpleTaxons)

parseEntrezSimpleTaxons :: ArrowXml a => a XmlTree SimpleTaxon
parseEntrezSimpleTaxons = getChildren >>> atTag "Taxon" >>>
  proc entrezSimpleTaxon -> do
  simple_TaxId <- atTag "TaxId" >>> getChildren >>> getText -< entrezSimpleTaxon
  simple_ScientificName <- atTag "ScientificName" >>> getChildren >>> getText -< entrezSimpleTaxon
  simple_ParentTaxId <- atTag "ParentTaxId" >>> getChildren >>> getText -< entrezSimpleTaxon
  simple_Rank <- atTag "Rank" >>> getChildren >>> getText -< entrezSimpleTaxon
  returnA -< SimpleTaxon {
    simpleTaxId = read simple_TaxId :: Int,
    simpleScientificName = TL.pack simple_ScientificName,
    simpleParentTaxId = read simple_ParentTaxId :: Int,
    simpleRank = read simple_Rank :: Rank
    }

readEntrezParentIds :: String -> [Int]
readEntrezParentIds = runLA (xreadDoc >>> parseEntrezParentTaxIds)

parseEntrezParentTaxIds :: ArrowXml a => a XmlTree Int
parseEntrezParentTaxIds = getChildren >>> atTag "Taxon" >>>
  proc entrezSimpleTaxon -> do
  simple_ParentTaxId <- atTag "ParentTaxId" >>> getChildren >>> getText -< entrezSimpleTaxon
  returnA -< read simple_ParentTaxId :: Int

-- | Read entrez summary from internal haskell string
readEntrezSummaries :: String -> [EntrezSummary]
readEntrezSummaries = runLA (xreadDoc >>> parseEntrezSummaries)

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
readEntrezSearch = runLA (xreadDoc >>> parseEntrezSearch)

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
    count = readInt _count,
    retMax = readInt _retMax,
    retStart = readInt _retStart,
    searchIds = _searchIds,
    translationStack = _translationStack,
    queryTranslation = _queryTranslation
    }

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

-- | Read entrez summary from internal haskell string
readEntrezGeneSummaries :: String -> [EntrezGeneSummary]
readEntrezGeneSummaries = runLA (xreadDoc >>> parseEntrezGeneSummaries)

-- | Parse entrez summary result
parseEntrezGeneSummaries :: ArrowXml a => a XmlTree EntrezGeneSummary
parseEntrezGeneSummaries = atTag "eSummaryResult" >>> getChildren >>> atTag "DocumentSummarySet" >>>
  proc entrezSummary -> do
  _geneSummaries <- listA parseEntrezGeneDocSums -< entrezSummary
  returnA -< EntrezGeneSummary {
    geneSummaries = _geneSummaries
    }

-- |
parseEntrezGeneDocSums :: ArrowXml a => a XmlTree EntrezGeneDocSummary
parseEntrezGeneDocSums = atTag "DocumentSummary" >>>
  proc entrezDocSum -> do
  _geneId <- atTag "Name" >>> getChildren >>> getText -< entrezDocSum
  _geneName <- atTag "Description" >>> getChildren >>> getText -< entrezDocSum
  _geneStatus <- atTag "Status" >>> getChildren >>> getText -< entrezDocSum
  _geneCurrentID <- atTag "CurrentID" >>> getChildren >>> getText -< entrezDocSum
  _geneGeneticSource <- atTag "GeneticSource" >>> getChildren >>> getText -< entrezDocSum
  _geneOtherAliases <- atTag "OtherAliases" >>> getChildren >>> getText -< entrezDocSum
  _geneGenomicInfo <- parseEntrezGenomicInfo -< entrezDocSum
  returnA -< EntrezGeneDocSummary {
    geneId = _geneId,
    geneName = _geneName,
    geneStatus = _geneStatus,
    geneCurrentID = _geneCurrentID,
    geneGeneticSource = _geneGeneticSource,
    geneOtherAliases = _geneOtherAliases,
    geneGenomicInfo = _geneGenomicInfo
    }

parseEntrezGenomicInfo :: ArrowXml a => a XmlTree EntrezGenomicInfo
parseEntrezGenomicInfo = atTag "GenomicInfo" >>> getChildren >>> atTag "GenomicInfoType" >>>
  proc entrezGenomicInfo -> do
  _chrAccVer <- atTag "ChrAccVer" >>> getChildren >>> getText -< entrezGenomicInfo
  _chrStart <- atTag "ChrStart" >>> getChildren >>> getText -< entrezGenomicInfo
  _chrStop <- atTag "ChrStop" >>> getChildren >>> getText -< entrezGenomicInfo
  _exonCount <- atTag "ExonCount" >>> getChildren >>> getText -< entrezGenomicInfo
  returnA -< EntrezGenomicInfo {
    chrAccVer = _chrAccVer,
    chrStart  = readInt _chrStart,
    chrStop = readInt _chrStop,
    exonCount = readInt _exonCount
    }


-- | gets all subtrees with the specified tag name
atTag :: ArrowXml a =>  String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

---------------------------------------
-- Retrieval functions

-- | Retrieve sequence for gene symbol (e.g. yhfA) from accession number (e.g. NC_000913.3) and if available entrez registration (toolname,devemail)
retrieveGeneSymbolFasta :: String -> String -> Maybe (String,String) -> IO String
retrieveGeneSymbolFasta genesymbol accession registrationInfo = do
  let query1 = EntrezHTTPQuery (Just "esearch") (Just "gene") ("term=" ++ genesymbol ++ urlEncode ("[Gene Name] AND " ++ accession ++ "[Nucleotide Accession]"))
  --print query1
  uniqueidresponse <- entrezHTTP query1
  --print uniqueidresponse
  let uniqueid = head (searchIds (head (readEntrezSearch uniqueidresponse)))
  let query2 = EntrezHTTPQuery (Just "esummary") (Just "gene") ("id=" ++ show uniqueid)
  summaryresponse <- entrezHTTP query2
  --print summaryresponse
  let parsedSummary = head (geneSummaries (head (readEntrezGeneSummaries summaryresponse)))
  let accessionVersion = chrAccVer (geneGenomicInfo parsedSummary)
  let seqStart = chrStart (geneGenomicInfo parsedSummary)
  let seqStop = chrStop (geneGenomicInfo parsedSummary)
  let strand = convertCoordinatesToStrand seqStart seqStop
  fetchNucleotideString accessionVersion seqStart seqStop strand registrationInfo


-- | Fetches sequence strings from the nucleotide database. nucleotideId can be a NCBI accession number or gene id.
--   Strand is 1 in case of plus strand (forward) or 2 minus (reverse) strand, the setStrand function can be used for conversion.
fetchNucleotideString :: String -> Int -> Int -> Int -> Maybe (String,String) -> IO String
fetchNucleotideString nucleotideId seqStart seqStop strand maybeRegistrationInfo = do
  let registrationInfo = maybeBuildRegistration maybeRegistrationInfo
  let program' = Just "efetch"
  let database' = Just "nucleotide"
  let query' = "id=" ++ nucleotideId ++ "&seq_start=" ++ show seqStart ++ "&seq_stop=" ++ show seqStop ++ "&rettype=fasta" ++ "&strand=" ++ show strand ++ registrationInfo
  let entrezQuery = EntrezHTTPQuery program' database' query'
  entrezHTTP entrezQuery

convertCoordinatesToStrand :: Int -> Int -> Int
convertCoordinatesToStrand start end
  | start <= end = 1
  | otherwise = 2

setStrand :: String -> Int
setStrand strandString
  | strandString == "+" = 1
  | strandString == "-" = 2
  | strandString == "forward" = 1
  | strandString == "reverse" = 2
  | strandString == "Forward" = 1
  | strandString == "Reverse" = 2
  | strandString == "plus" = 1
  | strandString == "false" = 2
  | strandString == "Plus" = 1
  | strandString == "False" = 2
  | otherwise = 1

readInt :: String -> Int
readInt = read

-- | Builds Entrez registration String if present
maybeBuildRegistration :: Maybe (String,String) -> String
maybeBuildRegistration maybeRegistration
  | isJust maybeRegistration = buildRegistration toolname developeremail
  | otherwise = ""
  where registration = fromJust maybeRegistration
        toolname = fst registration
        developeremail = snd registration

-- | Builds Entrez registration String that has to be appended to query key
buildRegistration :: String -> String -> String
buildRegistration toolname developeremail = "&tool=" ++ toolname ++ "&email=" ++ developeremail
