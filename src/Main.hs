{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString
import           Data.Time.Clock
import           Network.HTTP.Client
import           Network.HTTP.Conduit
import           Network.HTTP.Simple

buildAuthRequest :: ByteString -> ByteString -> IO Request
buildAuthRequest username password = do
  authRequest <- parseRequest "POST https://academic.ui.ac.id/main/Authentication/Index"
  return (setRequestSecure True
    $ setRequestBodyURLEncoded [("u", username), ("p", password)] authRequest)

buildChangeRoleRequest :: IO Request
buildChangeRoleRequest = do
  changeRoleRequest <- parseRequest "GET https://academic.ui.ac.id/main/Authentication/ChangeRole"
  return (setRequestSecure True changeRoleRequest)


main :: IO ()
main = do
  -- Make authentication request
  authRequest <- buildAuthRequest "widyanto.bagus" "lala123"
  authResponse <- httpLBS authRequest

  -- Create cookie jar with authentication cookies from auth request
  now <- getCurrentTime
  let emptyJar = createCookieJar []
  let authenticationCookieJar = fst $ updateCookieJar authResponse authRequest now emptyJar

  -- Make change role request
  changeRoleRequest <- buildChangeRoleRequest
  changeRoleResp <- httpLBS (changeRoleRequest { cookieJar = Just authenticationCookieJar })

  -- At this point, we have successfully authenticated.
  -- Use authenticationCookieJar for further requests.
  print (getResponseHeaders changeRoleResp)
  print (getResponseBody changeRoleResp)
  print authenticationCookieJar
