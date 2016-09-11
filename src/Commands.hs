module Commands where

import           Network.HTTP.Client  (parseRequest)
import           Network.HTTP.Conduit (CookieJar, cookieJar)
import           Network.HTTP.Simple  (httpLBS, setRequestBodyURLEncoded,
                                       setRequestSecure)

import           Endpoints            (summaryUrl)

buildSummaryRequest = do
  request <- parseRequest $ "GET " ++ summaryUrl
  return (setRequestSecure True request)

showSummary :: CookieJar -> IO ()
showSummary authenticationCookieJar = do
  request <- buildSummaryRequest
  response <- httpLBS $ request { cookieJar = Just authenticationCookieJar }

  print response
