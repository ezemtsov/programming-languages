{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Typeable
import Control.Exception
import Control.Monad (liftM)

import System.Environment
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import Text.RawString.QQ


import Database.SQLite.Simple
  hiding (close)
import qualified Database.SQLite.Simple
  as SQLite
import Database.SQLite.Simple.Types


data User =
  User {
      userId        :: Integer
    , username      :: Text
    , shell         :: Text
    , homeDirectory :: Text
    , realName      :: Text
    , phone         :: Text
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir
         realName phone) =
    toRow (id_, username, shell, homeDir,
           realName, phone)

formatUser :: User -> Text
formatUser (User _ username shell
            homeDir realName _) = mconcat
  [" Login: ", username,
   " Name: ", realName,
   " Directory: ", homeDir,
   " Shell: ", shell ]

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT, homeDirectory TEXT,
   realName TEXT, phone TEXT)
|]

insertUser :: Query
insertUser =
  "INSERT INTO users\
  \ VALUES (?, ?, ?, ?, ?, ?)"

alterUser :: Query
alterUser = [r|
UPDATE users
SET shell = ?,
    homeDirectory = ?,
    realName = ?,
    phone = ?
WHERE username = ?
|]

allUsers :: Query
allUsers =
  "SELECT * from users"

getUserQuery :: Query
getUserQuery =
  "SELECT * from users where username = ?"

data DuplicateData =
  DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

data UserAlreadyExists =
  UserAlreadyExists
  deriving (Eq, Show, Typeable)

instance Exception UserAlreadyExists

type UserRow =
  (Null, Text, Text, Text, Text, Text)

getUser :: Connection
        -> Text
        -> IO (Maybe User)
getUser conn username = do
  results <-
    query conn getUserQuery (Only username)
  case results of
    [] -> return $ Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

createUser :: Connection
           -> Text -> Text -> Text
           -> Text -> Text -> IO ()
createUser conn uname shell homeDir
           realName phone = do
  execute conn insertUser userRow
  (Just updatedUser) <- getUser conn uname
  print $ formatUser updatedUser
  where userRow :: UserRow
        userRow =
          (Null, uname, shell,
           homeDir, realName,
           phone)


type UpdateRow =
  (Text, Text, Text, Text, Text)
  
updateUser :: Connection
           -> Text -> Text -> Text
           -> Text -> Text -> IO ()
updateUser conn uname shell homeDir
           realName phone = do
  oldUser <- getUser conn uname
  execute conn alterUser updateRow
  (Just updatedUser) <- getUser conn uname
  print updatedUser
  where updateRow :: UpdateRow
        updateRow =
          (shell, homeDir, realName,
           phone, uname)

getArg :: [Text] -> Text -> Text
getArg args s = case valueIndex of
  (Just t) -> args !! (t + 1)
  Nothing -> ""
  where valueIndex = elemIndex s args

main :: IO ()
main = do
  args <- liftM (fmap T.pack) getArgs
  let username = getArg args "-p"
      shell = getArg args "-s"
      homeDir = getArg args "-h"
      realName = getArg args "-r"
      phone = getArg args "-P"
  conn <- open "finger.db"
  checkUser <- getUser conn username
  case checkUser of
    Nothing -> do
      putStrLn "User is new, inserting"
      createUser conn username
        shell homeDir realName phone
    Just _ -> do
      putStrLn "User already exists, updating"
      updateUser conn username
        shell homeDir realName phone
  SQLite.close conn
