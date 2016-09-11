{-# LANGUAGE OverloadedStrings #-}

module Auth where

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Time.Clock       (getCurrentTime)
import           Network.HTTP.Client   (parseRequest, updateCookieJar)
import           Network.HTTP.Conduit  (CookieJar, Request, cookieJar,
                                        createCookieJar)
import           Network.HTTP.Simple   (httpLBS, setRequestBodyURLEncoded,
                                        setRequestSecure)

import           Endpoints             (authenticationUrl, changeRoleUrl)

buildAuthRequest :: String -> String -> IO Request
buildAuthRequest username password = do
  authRequest <- parseRequest $ "POST " ++ authenticationUrl
  return (setRequestSecure True
    $ setRequestBodyURLEncoded [("u", pack username), ("p", pack password)] authRequest)

buildChangeRoleRequest :: IO Request
buildChangeRoleRequest = do
  changeRoleRequest <- parseRequest $ "GET " ++ changeRoleUrl
  return (setRequestSecure True changeRoleRequest)

authenticate :: String -> String -> IO CookieJar
authenticate username password = do
  -- Make authentication request
  authRequest <- buildAuthRequest username password
  authResponse <- httpLBS authRequest

  -- Create cookie jar with authentication cookies from auth request
  now <- getCurrentTime
  let emptyJar = createCookieJar []
  let authenticationCookieJar = fst $ updateCookieJar authResponse authRequest now emptyJar

  -- Make change role request
  changeRoleRequest <- buildChangeRoleRequest
  changeRoleResp <- httpLBS (changeRoleRequest { cookieJar = Just authenticationCookieJar })

  return authenticationCookieJar
