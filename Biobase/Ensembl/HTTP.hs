{-# LANGUAGE OverloadedStrings #-}
-- | Interface for Ensembl

module Biobase.Ensembl.HTTP (
                       startXRefSession,
                       requestGOTermsWithGeneIds,
                       requestUniProtWithGeneIds,
                       EnsemblEntry(..)
                      ) where

import Network.HTTP.Client
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.Socket
import Data.Aeson
import Data.Either.Unwrap
import Biobase.Ensembl.REST.Types
import Control.Concurrent
import Data.Maybe
import Data.List
import qualified Data.Text as T

requestGOTermsWithGeneIds :: [T.Text] -> IO [(T.Text,[T.Text])]
requestGOTermsWithGeneIds geneIds = do
  let queryParameters = T.pack "?external_db=GO;all_levels=1;content-type=application/json"
  mapM (requestGOTermsWithGeneId queryParameters) geneIds

requestGOTermsWithGeneId :: T.Text -> T.Text -> IO (T.Text,[T.Text])
requestGOTermsWithGeneId queryParameters geneId = do
  --let protid= "AAC73113"
  responseJson <- startXRefSession queryParameters geneId
  --3/s
  threadDelay 330000
  if isRight responseJson
    then return (geneId,(extractGOTerms (fromRight responseJson)))
    else return (geneId,[])

extractGOTerms :: [EnsemblEntry] -> [T.Text]
extractGOTerms entries = goTerms
  where goEntries = filter (\a -> T.pack "GO" == fromMaybe "" (dbname a)) entries
        goTerms = map (fromJust . display_id) goEntries

requestUniProtWithGeneIds :: [T.Text] -> IO [(T.Text,T.Text)]
requestUniProtWithGeneIds geneIds = do
  let queryParameters = T.pack "?external_db=Uniprot/SWISSPROT;all_levels=1;content-type=application/json"
  mapM (requestUniProtWithGeneId queryParameters) geneIds

requestUniProtWithGeneId :: T.Text -> T.Text -> IO (T.Text,T.Text)
requestUniProtWithGeneId queryParameters geneId = do
  --let protid= "AAC73113"
  responseJson <- startXRefSession queryParameters geneId
  -- 3/s
  threadDelay 330000
  if isRight responseJson
    then return (geneId,(extractUniProtId (fromRight responseJson)))
    else return (geneId,T.empty)

extractUniProtId :: [EnsemblEntry] -> T.Text
extractUniProtId entries = uniprotName
  where uniprotEntry = find (\a -> T.pack "Uniprot/SWISSPROT" == fromMaybe "" (dbname a)) entries
        uniprotName = if isJust uniprotEntry then fromMaybe T.empty (display_id (fromJust uniprotEntry)) else T.empty

startXRefSession :: T.Text -> T.Text -> IO (Either String [EnsemblEntry])
startXRefSession queryParameters xrefId = do
  --response <- withSocketsDo
  --    $ sendXRefQuery xrefId queryParameters
  let settings = managerSetProxy
            (proxyEnvironment Nothing)
            defaultManagerSettings
  man <- newManager settings
  request <- parseRequest ("http://rest.ensemblgenomes.org/xrefs/id/" ++ T.unpack xrefId ++ T.unpack queryParameters)
  response <- httpLbs request man
  let rbody = responseBody response
  let decodedJson = eitherDecode rbody :: Either String [EnsemblEntry]
  return decodedJson

--"[{\"display_id\":\"00300+2.7.2.4+1.1.1.3\",\"primary_id\":\"00300+2.7.2.4+1.1.1.3\",\"version\":\"0\",\"description\":\"\",\"dbname\":\"KEGG_Enzyme\",\"synonyms\":[],\"info_text\":\"\",\"info_type\":\"NONE\",\"db_display_name\":\"KEGG Pathway and Enzyme\"}]""

-- | Send query and return response
-- http://rest.ensemblgenomes.org/xrefs/id/AAC73113?content-type=application/json
--sendXRefQuery :: T.Text -> T.Text -> IO L8.ByteString
--sendXRefQuery xrefId queryParameters = simpleHttp ("http://rest.ensemblgenomes.org/xrefs/id/" ++ T.unpack xrefId ++ T.unpack queryParameters)
