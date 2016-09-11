{-# LANGUAGE OverloadedStrings #-}

module Auth where

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Time.Clock       (getCurrentTime)
import           Endpoints             (authenticationUrl, changeRoleUrl)
import           Network.HTTP.Client   (parseRequest, updateCookieJar)
import           Network.HTTP.Conduit  (CookieJar, Request, cookieJar,
                                        createCookieJar)
import           Network.HTTP.Simple   (httpLBS, setRequestBodyURLEncoded,
                                        setRequestSecure)
import           System.Environment    (getEnv)

buildAuthRequest :: ByteString -> ByteString -> IO Request
buildAuthRequest username password = do
  authRequest <- parseRequest $ "POST " ++ authenticationUrl
  return (setRequestSecure True
    $ setRequestBodyURLEncoded [("u", username), ("p", password)] authRequest)

buildChangeRoleRequest :: IO Request
buildChangeRoleRequest = do
  changeRoleRequest <- parseRequest $ "GET " ++ changeRoleUrl
  return (setRequestSecure True changeRoleRequest)

getAuthenticationCookieJar :: IO CookieJar
getAuthenticationCookieJar = do
  username <- getEnv "SIAK_USERNAME"
  password <- getEnv "SIAK_PASSWORD"

  putStrLn $ "Authenticating with credentials of " ++ username ++ "..."

  -- Make authentication request
  authRequest <- buildAuthRequest (pack username) (pack password)
  authResponse <- httpLBS authRequest

  -- Create cookie jar with authentication cookies from auth request
  now <- getCurrentTime
  let emptyJar = createCookieJar []
  let authenticationCookieJar = fst $ updateCookieJar authResponse authRequest now emptyJar

  -- Make change role request
  changeRoleRequest <- buildChangeRoleRequest
  changeRoleResp <- httpLBS (changeRoleRequest { cookieJar = Just authenticationCookieJar })

  return authenticationCookieJar
