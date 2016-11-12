--Testscript
--runghc -package-db --ghc-arg=.cabal-sandbox/x86_64-linux-ghc-8.0.1-packages.conf.d/  EntrezHTTPtest.hs
-- needs cmdargs and biofasta packages
{-# LANGUAGE Arrows #-}
module Main where
    
import System.Console.CmdArgs
import Bio.EntrezHTTP
import Bio.Sequence.Fasta
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B
import Text.XML.HXT.Core
import Bio.Core.Sequence 

-- | gets all subtrees with the specified tag name
atTag :: ArrowXml a =>  String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

efetchTaxonomyTest :: IO ()
efetchTaxonomyTest = do
  let program = Just "efetch"
  let database = Just "taxonomy" 
  let queryString = "id=405955,1299281"
  let entrezQuery = EntrezHTTPQuery program database queryString 
  let testresult3= "<?xml version=\"1.0\"?><!DOCTYPE TaxaSet PUBLIC \"-//NLM//DTD Taxon, 14th January 2002//EN\" \"http://www.ncbi.nlm.nih.gov/entrez/query/DTD/taxon.dtd\"><TaxaSet><Taxon><TaxId>1406860</TaxId><ScientificName>Salmonella</ScientificName></Taxon></TaxaSet>"
--  <ScientificName>Salmonella enterica subsp. enterica serovar Agona str. 24249</ScientificName>
  result <- entrezHTTP entrezQuery
  --let parentTaxIds = readEntrezParentIds result
  let taxons = readEntrezTaxonSet result
  --print parentTaxIds
  --print result
  print testresult3
  print taxons

efetchSequenceTest :: IO ()
efetchSequenceTest = do
  let program = Just "efetch"
  let database = Just "nucleotide" 
  let queryString = "id=556503834&seq_start=10&seq_stop=40&rettype=fasta"
  let entrezQuery = EntrezHTTPQuery program database queryString 
  result <- entrezHTTP entrezQuery
  let parsedFasta = (mkSeqs . B.lines) (B.pack result)
  --let summary = head (readEntrezSummaries result)
  print (seqid (head (parsedFasta)))
  --print (take 10 (drop 10 (toStr (seqdata (head (parsedFasta))))))
  print (toStr (seqdata (head (parsedFasta))))

esummaryTest :: IO ()
esummaryTest = do
  let program = Just "esummary"
  let database = Just "nucleotide" 
  let queryString = "id=556503834"
  let entrezQuery = EntrezHTTPQuery program database queryString 
  result <- entrezHTTP entrezQuery

  let testresult = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE eSummaryResult PUBLIC \"-//NLM//DTD esummary v1 20060131//EN\" \"http://eutils.ncbi.nlm.nih.gov/eutils/dtd/20060131/esummary-v1.dtd\">\n<eSummaryResult>\n<DocSum>\n\t<Id>556503834</Id>\n\t<Item Name=\"Caption\" Type=\"String\">NC_000913</Item>\n\t<Item Name=\"Title\" Type=\"String\">Escherichia coli str. K-12 substr. MG1655, complete genome</Item>\n\t<Item Name=\"Extra\" Type=\"String\">gi|556503834|ref|NC_000913.3||gnl|NCBI_GENOMES|115[556503834]</Item>\n\t<Item Name=\"Gi\" Type=\"Integer\">556503834</Item>\n\t<Item Name=\"CreateDate\" Type=\"String\">2001/10/15</Item>\n\t<Item Name=\"UpdateDate\" Type=\"String\">2014/03/19</Item>\n\t<Item Name=\"Flags\" Type=\"Integer\">800</Item>\n\t<Item Name=\"TaxId\" Type=\"Integer\">511145</Item>\n\t<Item Name=\"Length\" Type=\"Integer\">4641652</Item>\n\t<Item Name=\"Status\" Type=\"String\">live</Item>\n\t<Item Name=\"ReplacedBy\" Type=\"String\"></Item>\n\t<Item Name=\"Comment\" Type=\"String\"><![CDATA[  ]]></Item>\n</DocSum>\n\n</eSummaryResult>\n"

  let testresult2 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE eSummaryResult PUBLIC \"-//NLM//DTD esummary v1 20060131//EN\" \"http://eutils.ncbi.nlm.nih.gov/eutils/dtd/20060131/esummary-v1.dtd\"><eSummaryResult><DocSum><Id>556503834</Id><Item Name=\"Caption\" Type=\"String\">NC_000913</Item><Item Name=\"Title\" Type=\"String\">Escherichia coli str. K-12 substr. MG1655, complete genome</Item><Item Name=\"Extra\" Type=\"String\">gi|556503834|ref|NC_000913.3||gnl|NCBI_GENOMES|115[556503834]</Item><Item Name=\"Gi\" Type=\"Integer\">556503834</Item><Item Name=\"CreateDate\" Type=\"String\">2001/10/15</Item><Item Name=\"UpdateDate\" Type=\"String\">2014/03/19</Item><Item Name=\"Flags\" Type=\"Integer\">800</Item>\n\t<Item Name=\"TaxId\" Type=\"Integer\">511145</Item><Item Name=\"Length\" Type=\"Integer\">4641652</Item><Item Name=\"Status\" Type=\"String\">live</Item><Item Name=\"ReplacedBy\" Type=\"String\"></Item><Item Name=\"Comment\" Type=\"String\"><![CDATA[  ]]></Item></DocSum></eSummaryResult>"

  


  --let summary = runLA  (xreadDoc >>> getEntrezSummary ) result 
  let summary = head (readEntrezSummaries result)
  print summary

main = do
  efetchTaxonomyTest

