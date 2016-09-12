{-# LANGUAGE OverloadedStrings #-}

module Commands where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Char
import           Network.HTTP.Client           (parseRequest, Request)
import           Network.HTTP.Conduit          (CookieJar, cookieJar)
import           Network.HTTP.Simple           (getResponseBody, httpLBS,
                                                setRequestBodyURLEncoded,
                                                setRequestSecure)
import           Text.HTML.TagSoup

import           Endpoints                     (summaryUrl)

type Key = B.ByteString
type Value = B.ByteString

data Entry = Entry Key Value deriving (Show)

buildSummaryRequest :: IO Request
buildSummaryRequest = do
  request <- parseRequest $ "GET " ++ summaryUrl
  return (setRequestSecure True request)

containsWhitespace :: B.ByteString -> Bool
containsWhitespace = B.any (== (fromIntegral $ ord '\n'))

makePairs :: [B.ByteString] -> [(B.ByteString, B.ByteString)]
makePairs [] = []
makePairs [_] = []
makePairs (a:b:xs) = (a, b) : makePairs xs

pairsToEntries :: [(B.ByteString, B.ByteString)] -> [Entry]
pairsToEntries [] = []
pairsToEntries ((a, b):xs) = Entry a b : pairsToEntries xs

printEntry :: Entry -> IO ()
printEntry (Entry key value) =
  B8.putStrLn $ B.intercalate ": " [key, value]

printEntries :: [Entry] -> IO ()
printEntries [] = return ()
printEntries (entry:restOfEntries) = do
  printEntry entry
  printEntries restOfEntries

parseSummary :: B.ByteString -> [Entry]
parseSummary html =
  pairsToEntries $ makePairs withoutWhitespace
  where
    tags = parseTags html
    summaryTable =
          takeWhile (~/= TagComment (" Grafik IP/IPK " :: B.ByteString)) $
          dropWhile (~/= TagComment (" Summary " :: B.ByteString)) tags
    texts = map fromTagText $ filter isTagText summaryTable
    withoutWhitespace = filter (not . containsWhitespace) texts

showSummary :: CookieJar -> IO ()
showSummary authenticationCookieJar = do
  putStrLn "Fetching summary..."

  request <- buildSummaryRequest
  response <- httpLBS $ request { cookieJar = Just authenticationCookieJar }

  putStrLn "Fetched summary."

  let responseBody = getResponseBody response
  let entries = parseSummary responseBody

  putStrLn ""
  putStrLn ""
  putStrLn "======== Summary ========"
  printEntries entries
  putStrLn "========   End   ========"
