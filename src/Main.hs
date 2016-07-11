{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Configuration.Dotenv  as D
import qualified Data.ByteString.Char8 as B
import           Data.Time.Clock
import           Network.HTTP.Client
import           Network.HTTP.Conduit
import           Network.HTTP.Simple
import           System.Environment

buildAuthRequest :: B.ByteString -> B.ByteString -> IO Request
buildAuthRequest username password = do
  authRequest <- parseRequest "POST https://academic.ui.ac.id/main/Authentication/Index"
  return (setRequestSecure True
    $ setRequestBodyURLEncoded [("u", username), ("p", password)] authRequest)

buildChangeRoleRequest :: IO Request
buildChangeRoleRequest = do
  changeRoleRequest <- parseRequest "GET https://academic.ui.ac.id/main/Authentication/ChangeRole"
  return (setRequestSecure True changeRoleRequest)

getAuthenticationCookieJar :: IO CookieJar
getAuthenticationCookieJar = do
  username <- getEnv "SIAK_USERNAME"
  password <- getEnv "SIAK_PASSWORD"

  putStrLn $ "Authenticating with credentials of " ++ username ++ "..."

  -- Make authentication request
  authRequest <- buildAuthRequest (B.pack username) (B.pack password)
  authResponse <- httpLBS authRequest

  -- Create cookie jar with authentication cookies from auth request
  now <- getCurrentTime
  let emptyJar = createCookieJar []
  let authenticationCookieJar = fst $ updateCookieJar authResponse authRequest now emptyJar

  -- Make change role request
  changeRoleRequest <- buildChangeRoleRequest
  changeRoleResp <- httpLBS (changeRoleRequest { cookieJar = Just authenticationCookieJar })

  return authenticationCookieJar

main :: IO ()
main = do
  -- Load environment variables
  D.loadFile False ".env"

  authenticationCookieJar <- getAuthenticationCookieJar

  -- At this point, we have successfully authenticated.
  -- Use authenticationCookieJar for further requests.
  print authenticationCookieJar
