{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Exception
  hiding (Handler)
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
  (decodeUtf8, encodeUtf8)

import Data.Typeable
import Database.SQLite.Simple
  hiding (close, bind)
import qualified Database.SQLite.Simple
  as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (recv)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (split)
import qualified Data.ByteString as BS
import Network.Socket.ByteString
       (recv, sendAll)
import Text.RawString.QQ

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

type Port = Int

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

deleteUser :: Query
deleteUser =
  "DELETE FROM users WHERE username = ?"

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
  "SELECT * from users WHERE username = ?"

data DuplicateData =
  DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow =
  (Null, Text, Text, Text, Text, Text)

getUser :: Connection
        -> Text
        -> IO (Maybe User)
getUser conn uname = do
  results <-
    query conn getUserQuery (Only uname)
  case results of
    [] -> return $ Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser meRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where meRow :: UserRow
        meRow =
          (Null, "ezemtsov", "/bin/fish",
          "home/ezemtsov", "Evgeny Zemtsov",
          "463-97-700")

returnUsers :: Connection
            -> Socket
            -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers
  let usernames = map username rows
      newlineSeparated =
        T.concat $
        intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ uname shell
            homeDir realName _) = BS.concat
  ["Login: ", e uname, "\t\t\t\t",
   "Name: ", e realName, "\n",
   "Directory: ", e homeDir, "\t\t\t",
   "Shell: ", e shell, "\n"]
  where e = encodeUtf8

returnUser :: Connection
           -> Socket
           -> Text
           -> IO ()
returnUser dbConn soc uname = do
  maybeUser <-
    getUser dbConn (T.strip uname)
  case maybeUser of
    Nothing -> do
      putStrLn
        ("Couldn't find matching user\
         \ for username: "
         ++ (show uname))
      return ()
    Just user ->
      sendAll soc (formatUser user)
      
type UpdateRow =
  (Text, Text, Text, Text, Text)

updateUser :: Connection
           -> Socket
           -> Text -> Text -> Text
           -> Text -> Text -> IO ()
updateUser dbConn soc uname shell homeDir
           realName phone = do
  maybeUser <-
    getUser dbConn (T.strip uname)  
  case maybeUser of
    Nothing -> do
      execute dbConn insertUser userRow
      sendAll soc "User created"
    Just _ -> do
      execute dbConn alterUser updateRow
      sendAll soc "User updated"
  where updateRow :: UpdateRow
        updateRow =
          (shell, homeDir, realName,
           phone, uname)
        userRow :: UserRow
        userRow =
          (Null, uname, shell,
           homeDir, realName,
           phone)
          
dropUser :: Connection
         -> Socket
         -> Text
         -> IO ()
dropUser dbConn soc uname = do
  maybeUser <- getUser dbConn uname
  case maybeUser of
    Nothing -> do
      putStrLn
        ("Couln't find matching user\
         \ for username: "
         ++ (show uname))
      return ()
    Just _ -> do
      execute dbConn deleteUser (Only uname)
      sendAll soc "User deleted"

handleFingerd :: Connection
              -> Socket
              -> IO ()
handleFingerd dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name -> returnUser dbConn soc (decodeUtf8 name)

handleControl :: Connection
              -> Socket
              -> IO ()
handleControl dbConn soc = do
  msg <- recv soc 1024
  case (split '|' msg) of
      ["list", _] -> returnUsers dbConn soc
      ["show", name, _] -> returnUser dbConn soc (d name)
      ["delete", name, _] -> dropUser dbConn soc (d name)
      ["alter", uname, shell,
       homeDir, realName, phone, _]
        -> updateUser dbConn soc (d uname)
           (d shell) (d homeDir)
           (d realName) (d phone)
      _ -> sendAll soc "Wrong command"
  where d = decodeUtf8

handleQueries :: Connection
              -> Socket
              -> Mode
              -> Port
              -> IO ()
handleQueries dbConn sock mode port = forever $ do
  (soc, _) <- accept sock
  putStrLn $ mconcat ["Got connection on port ",
                      show port,
                      ", handling query"]
  case mode of
    Consume -> handleFingerd dbConn soc
    Control -> handleControl dbConn soc
  close soc

data Mode = Consume | Control
  deriving (Eq, Show)

startServer :: Mode -> Port -> IO ()
startServer mode port = withSocketsDo $ do
  addrinfos <-
    getAddrInfo
    (Just (defaultHints
          {addrFlags = [AI_PASSIVE]}))
    Nothing (Just $ show port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr)
          Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1
  -- only one connection open at a time
  putStrLn ("Server succesfully started on Port: "
            ++ (show port))
  conn <- open "finger.db"
  handleQueries conn sock mode port
  SQLite.close conn
  close sock

main :: IO ()
main = forkIO (startServer Control 78)
       >> startServer Consume 79
