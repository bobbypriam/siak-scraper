{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Time.Clock
import           Network.HTTP.Client
import           Network.HTTP.Conduit
import           Network.HTTP.Simple

main :: IO ()
main = do
  authReq' <- parseRequest "POST https://academic.ui.ac.id/main/Authentication/Index"
  let authReq = setRequestSecure True
              $ setRequestBodyURLEncoded [("u", "widyanto.bagus"), ("p", "lala123")] authReq'
  authResp <- httpLBS authReq
  now <- getCurrentTime
  let newJar = fst $ updateCookieJar authResp authReq now (createCookieJar [])
  print (getResponseHeaders authResp)
  changeRoleReq' <- parseRequest "GET https://academic.ui.ac.id/main/Authentication/ChangeRole"
  let changeRoleReq = setRequestSecure True
              $ authReq' { cookieJar = Just newJar }
  changeRoleResp <- httpLBS changeRoleReq
  print (getResponseBody changeRoleResp)
