module Main where

import           Configuration.Dotenv (loadFile)
import           System.Directory     (doesFileExist)
import           System.Environment   (getArgs, getEnv)

import           Auth                 (authenticate)

main :: IO ()
main = do
  envExists <- doesFileExist ".env"

  if envExists
    then putStrLn "Reading configuration from .env"
    else error "No .env file found! Use the provided .env"

  -- Load environment variables
  loadFile False ".env"

  args <- getArgs

  if null args
    then do
      printHelp
      error "No command given."
    else do
      let command = head args

      if not (isValidCommand command)
        then do
          printHelp
          error $ "Invalid command: " ++ command
        else do
          putStrLn $ "Running " ++ command ++ " command."

          username <- getEnv "SIAK_USERNAME"
          password <- getEnv "SIAK_PASSWORD"

          putStrLn $ "Authenticating with credentials of " ++ username ++ "..."

          authenticationCookieJar <- authenticate username password

          -- At this point, we have successfully authenticated.
          -- Use authenticationCookieJar for further requests.
          case command of
            "summary" ->
              error "TODO: summary"
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
