module Main where

import           Configuration.Dotenv (loadFile)
import           Control.Monad        (unless, when)
import           System.Directory     (doesFileExist)
import           System.Environment   (getArgs, getEnv)

import           Action.Summary       (showSummary)
import           Auth                 (authenticate, isAuthenticationSuccess)

main :: IO ()
main = do
  envExists <- doesFileExist ".env"

  if envExists
    then putStrLn ".env file exists. Using it as config file."
    else error "No .env file found! Use the provided .env.example for pointers."

  -- Load environment variables
  loadFile False ".env"

  username <- getEnv "SIAK_USERNAME"
  password <- getEnv "SIAK_PASSWORD"

  putStrLn $ "Using credentials of " ++ username ++ "."

  args <- getArgs

  when (null args) $
    do printHelp
       error "No command given."

  let command = head args

  unless (isValidCommand command) $
    do printHelp
       error $ "Invalid command: " ++ command

  putStrLn "Authenticating..."

  authenticationCookieJar <- authenticate username password

  unless (isAuthenticationSuccess authenticationCookieJar) $
    error "Authentication failed. Possibly you provided wrong username or password."

  putStrLn "Authentication success."

  -- At this point, we have successfully authenticated.
  -- Use authenticationCookieJar for further requests.
  case command of
    "summary" ->
      showSummary authenticationCookieJar
    "grades" ->
      error "TODO: grades"
    "fees" ->
      error "TODO: fees"

isValidCommand :: String -> Bool
isValidCommand command
  | command == "summary"  = True
  | command == "grades"   = True
  | command == "fees"     = True
  | otherwise             = False

printHelp :: IO ()
printHelp =
  putStrLn $ concat
    [ "Usage:\n"
    , "   scraper [command]\n"
    , "\n"
    , "   Available commands:\n"
    , "       summary     - Get a summary of your student status.\n"
    , "       grades      - Get your grades for this semester.\n"
    , "       fees        - Get your last fee payment status.\n"
    ]
