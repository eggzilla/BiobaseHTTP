{-# LANGUAGE Arrows #-}
module Main where
    
import System.Console.CmdArgs
import Bio.EntrezHTTP
import qualified Data.Map as M
import Text.XML.HXT.Core

main = do
  let program = Just "esummary"
  let database = Just "nucleotide" 
  let queryString = "id=556503834"
  let entrezQuery = EntrezHTTPQuery program database queryString 
  result <- entrezHTTP entrezQuery

  let testresult = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE eSummaryResult PUBLIC \"-//NLM//DTD esummary v1 20060131//EN\" \"http://eutils.ncbi.nlm.nih.gov/eutils/dtd/20060131/esummary-v1.dtd\">\n<eSummaryResult>\n<DocSum>\n\t<Id>556503834</Id>\n\t<Item Name=\"Caption\" Type=\"String\">NC_000913</Item>\n\t<Item Name=\"Title\" Type=\"String\">Escherichia coli str. K-12 substr. MG1655, complete genome</Item>\n\t<Item Name=\"Extra\" Type=\"String\">gi|556503834|ref|NC_000913.3||gnl|NCBI_GENOMES|115[556503834]</Item>\n\t<Item Name=\"Gi\" Type=\"Integer\">556503834</Item>\n\t<Item Name=\"CreateDate\" Type=\"String\">2001/10/15</Item>\n\t<Item Name=\"UpdateDate\" Type=\"String\">2014/03/19</Item>\n\t<Item Name=\"Flags\" Type=\"Integer\">800</Item>\n\t<Item Name=\"TaxId\" Type=\"Integer\">511145</Item>\n\t<Item Name=\"Length\" Type=\"Integer\">4641652</Item>\n\t<Item Name=\"Status\" Type=\"String\">live</Item>\n\t<Item Name=\"ReplacedBy\" Type=\"String\"></Item>\n\t<Item Name=\"Comment\" Type=\"String\"><![CDATA[  ]]></Item>\n</DocSum>\n\n</eSummaryResult>\n"

  let testresult2 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE eSummaryResult PUBLIC \"-//NLM//DTD esummary v1 20060131//EN\" \"http://eutils.ncbi.nlm.nih.gov/eutils/dtd/20060131/esummary-v1.dtd\"><eSummaryResult><DocSum><Id>556503834</Id><Item Name=\"Caption\" Type=\"String\">NC_000913</Item><Item Name=\"Title\" Type=\"String\">Escherichia coli str. K-12 substr. MG1655, complete genome</Item><Item Name=\"Extra\" Type=\"String\">gi|556503834|ref|NC_000913.3||gnl|NCBI_GENOMES|115[556503834]</Item><Item Name=\"Gi\" Type=\"Integer\">556503834</Item><Item Name=\"CreateDate\" Type=\"String\">2001/10/15</Item><Item Name=\"UpdateDate\" Type=\"String\">2014/03/19</Item><Item Name=\"Flags\" Type=\"Integer\">800</Item>\n\t<Item Name=\"TaxId\" Type=\"Integer\">511145</Item><Item Name=\"Length\" Type=\"Integer\">4641652</Item><Item Name=\"Status\" Type=\"String\">live</Item><Item Name=\"ReplacedBy\" Type=\"String\"></Item><Item Name=\"Comment\" Type=\"String\"><![CDATA[  ]]></Item></DocSum></eSummaryResult>"

  --let summary = runLA  (xreadDoc >>> getEntrezSummary ) result 
  let summary = readEntrezSummary result
  print summary


-- | gets all subtrees with the specified tag name
atTag :: ArrowXml a =>  String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)
