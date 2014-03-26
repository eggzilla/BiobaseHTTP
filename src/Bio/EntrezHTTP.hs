{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

-- | Interface for the NCBI Entrez REST webservice
module Bio.EntrezHTTP ( EntrezHTTPQuery (..),
                       entrezHTTP,
                       getEntrezSummary,
                       getSummaryItem
                      ) where

import Network.HTTP.Conduit 
import Data.Conduit    
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad.IO.Class (liftIO)    
import qualified Control.Monad as CM
import Text.XML.HXT.Core
import Network
import qualified Data.Conduit.List as CL
import Data.List
import Control.Monad.Error as CM
import Control.Concurrent
import Data.Maybe
import Data.Either
import Bio.EntrezHTTPData

data EntrezHTTPQuery = EntrezHTTPQuery 
  { program :: Maybe String
  , database :: Maybe String
  , query :: String 
  }
  deriving (Show, Eq)

-- | Parse HTML results into Xml Tree datastructure
parseHTML :: String -> IOStateArrow s0 b0 XmlTree
parseHTML = readString [withParseHTML yes, withWarnings no] 
-- | Gets all subtrees with the specified id attribute
atName :: ArrowXml a => String -> a XmlTree XmlTree
atName elementId = deep (isElem >>> hasAttrValue "name" (== elementId))

-- | Gets all subtrees with the specified id attribute
atId :: ArrowXml a =>  String -> a XmlTree XmlTree
atId elementId = deep (isElem >>> hasAttrValue "id" (== elementId))
      
-- | Send query and parse return XML 
startSession :: String -> String -> String -> IO String
startSession program database query = do
  requestXml <- withSocketsDo
      $ sendQuery program database query
  let requestXMLString = L8.unpack requestXml
  return requestXMLString

-- | Send query and return response XML
sendQuery :: String -> String -> String -> IO L8.ByteString
sendQuery program database query = simpleHttp ("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"++ program ++ ".fcgi?" ++ "db=" ++ database ++ "&" ++ query)         

-- |
entrezHTTP :: EntrezHTTPQuery -> IO String
entrezHTTP (EntrezHTTPQuery program database query) = do
  let defaultProgram = "summary"
  let defaultDatabase = "nucleotide"                  
  let selectedProgram = fromMaybe defaultProgram program
  let selectedDatabase = fromMaybe defaultDatabase database  
  startSession selectedProgram selectedDatabase query

-- | Parse entrez summary result
getEntrezSummary :: ArrowXml a => a XmlTree EntrezSummary
getEntrezSummary = atTag "eSummaryResult" >>> 
  proc entrezSummary -> do
  document_Summaries <- listA getEntrezDocSum -< entrezSummary
  returnA -< EntrezSummary {
    documentSummaries = document_Summaries
    }     

-- | 
getEntrezDocSum :: ArrowXml a => a XmlTree EntrezDocSum
getEntrezDocSum = atTag "DocSum" >>> 
  proc entrezDocSum -> do
  summary_Id <- atTag "Id" >>> getChildren >>> getText -< entrezDocSum
  summary_Items <- listA getSummaryItem -< entrezDocSum
  returnA -< EntrezDocSum {
    summaryId = summary_Id,
    summaryItems = summary_Items
    } 

-- | 
getSummaryItem :: ArrowXml a => a XmlTree SummaryItem
getSummaryItem = atTag "Item" >>> 
  proc summaryItem -> do
  item_Name <- getAttrValue "Name" -< summaryItem
  item_Type <- getAttrValue "Type" -< summaryItem
  item_Content <- getText <<< getChildren -< summaryItem
  returnA -< SummaryItem {
    itemName = item_Name,
    itemType = item_Type,
    itemContent = item_Content
    } 

-- | gets all subtrees with the specified tag name
atTag :: ArrowXml a =>  String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)
