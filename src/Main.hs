module Main where

import           Configuration.Dotenv (loadFile)
import           System.Environment   (getEnv)

import           Auth                 (authenticate)

main :: IO ()
main = do
  -- Load environment variables
  loadFile False ".env"
  username <- getEnv "SIAK_USERNAME"
  password <- getEnv "SIAK_PASSWORD"

  putStrLn $ "Authenticating with credentials of " ++ username ++ "..."

  authenticationCookieJar <- authenticate username password

  -- At this point, we have successfully authenticated.
  -- Use authenticationCookieJar for further requests.
  print authenticationCookieJar
