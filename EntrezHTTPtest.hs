module Main where
    
import System.Console.CmdArgs
import Bio.EntrezHTTP

main = do
  let program = Just "esummary"
  let database = Just "nucleotide" 
  let queryString = "id=556503834"
  let entrezQuery = EntrezHTTPQuery program database queryString 
  result <- entrezHTTP entrezQuery
  print result
