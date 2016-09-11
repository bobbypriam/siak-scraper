module Main where

import           Configuration.Dotenv (loadFile)

import           Auth                 (getAuthenticationCookieJar)

main :: IO ()
main = do
  -- Load environment variables
  loadFile False ".env"

  authenticationCookieJar <- getAuthenticationCookieJar

  -- At this point, we have successfully authenticated.
  -- Use authenticationCookieJar for further requests.
  print authenticationCookieJar
