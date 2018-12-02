{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
-- | Interface for Ensembl

module Biobase.Ensembl.HTTP (
                       startXRefSession,
                       requestGOTermsWithGeneIds,
                       requestUniProtWithGeneIds,
                       EnsemblEntry(..)
                      ) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.Socket
import Data.Aeson
import Data.Either.Unwrap
import Biobase.Ensembl.REST.Types
import Control.Concurrent
import Data.Maybe
import Data.List

requestGOTermsWithGeneIds :: [String] -> IO [[String]]
requestGOTermsWithGeneIds geneIds = do
  let queryParameters = "?external_db=GO;all_levels=1;content-type=application/json"
  uniprotIds <- mapM (requestGOTermsWithGeneId queryParameters) geneIds
  return uniprotIds

requestGOTermsWithGeneId :: String -> String -> IO [String]
requestGOTermsWithGeneId queryParameters geneId = do
  --let protid= "AAC73113"
  responseJson <- startXRefSession queryParameters geneId
  --3/s
  threadDelay 330000
  if (isRight responseJson)
    then return (extractGOTerms (fromRight responseJson))
    else return []

extractGOTerms :: [EnsemblEntry] -> [String]
extractGOTerms entries = goTerms
  where goEntries = filter (\a -> "GO" == (fromMaybe "" (dbname a))) entries
        goTerms = map (fromJust . display_id) goEntries

requestUniProtWithGeneIds :: [String] -> IO [String]
requestUniProtWithGeneIds geneIds = do
  let queryParameters = "content-type=application/json"
  uniprotIds <- mapM (requestUniProtWithGeneId queryParameters) geneIds
  return uniprotIds

requestUniProtWithGeneId :: String -> String -> IO String
requestUniProtWithGeneId queryParameters geneId = do
  --let protid= "AAC73113"
  responseJson <- startXRefSession queryParameters geneId
  -- 3/s
  threadDelay 330000
  if (isRight responseJson)
    then return (extractUniProtId (fromRight responseJson))
    else return []

extractUniProtId :: [EnsemblEntry] -> String
extractUniProtId entries = uniprotName
  where uniprotEntry = find (\a -> "Uniprot/SWISSPROT" == (fromMaybe "" (dbname a))) entries
        uniprotName = if isJust uniprotEntry then fromMaybe [] (display_id (fromJust uniprotEntry)) else []

startXRefSession :: String -> String -> IO (Either String [EnsemblEntry])
startXRefSession queryParameters xrefId = do
  response <- withSocketsDo
      $ sendXRefQuery xrefId queryParameters
  let decodedJson = eitherDecode response :: Either String [EnsemblEntry]
  return decodedJson

--"[{\"display_id\":\"00300+2.7.2.4+1.1.1.3\",\"primary_id\":\"00300+2.7.2.4+1.1.1.3\",\"version\":\"0\",\"description\":\"\",\"dbname\":\"KEGG_Enzyme\",\"synonyms\":[],\"info_text\":\"\",\"info_type\":\"NONE\",\"db_display_name\":\"KEGG Pathway and Enzyme\"}]""

-- | Send query and return response
-- http://rest.ensemblgenomes.org/xrefs/id/AAC73113?content-type=application/json
sendXRefQuery :: String -> String -> IO L8.ByteString
sendXRefQuery xrefId queryParameters = do
  response <- simpleHttp ("http://rest.ensemblgenomes.org/xrefs/id/" ++ xrefId ++ queryParameters)
  return response
