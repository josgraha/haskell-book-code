{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Exception (Exception, bracket, throwIO)
import           Control.Monad (forever)
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           Data.List (intersperse)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Typeable (Typeable)
import qualified Database.SQLite.Simple as SQLite
import qualified Database.SQLite.Simple.Types as SQLite
import           Network.Socket hiding (close, recv)
import           Network.Socket.ByteString (recv, sendAll)
import           System.Environment (getArgs)

import Queries
import User

-- <Configuration>

fingerPort :: Int
fingerPort = 79

maxMessageLength :: Int
maxMessageLength = 1024

dbFileName :: String
dbFileName = "fingerd.db"

-- </Configuration>

createDatabase :: IO ()
createDatabase = do
    SQLite.withConnection dbFileName $ \db -> do
        SQLite.execute_ db createUsers
        SQLite.execute db insertUser meRow
        rows <- SQLite.query_ db allUsers
        mapM_ print (rows :: [User])
    where
        meRow :: UserRow
        meRow = (SQLite.Null, "rcook", "/bin/bash", "/home/rcook", "Richard Cook", "555-123-4567")

data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)

instance Exception DuplicateData

getUser :: SQLite.Connection -> Text -> IO (Maybe User)
getUser db u = do
    results <- SQLite.query db fetchUser (SQLite.Only u)
    case results of
        [] -> return Nothing
        [user] -> return $ Just user
        _ -> throwIO DuplicateData

formatUser :: User -> ByteString
formatUser User{..} =
    BS.concat
        [ "User name: "
        , e userName
        , "\n"
        , "Name: "
        , e fullName
        , "\n"
        , "Home directory: "
        , e homeDir
        , "\n"
        , "Shell: "
        , e shell
        , "\n"
        ]
    where e = encodeUtf8

returnUser :: Socket -> SQLite.Connection -> Text -> IO ()
returnUser soc db u = do
    maybeUser <- getUser db (T.strip u)
    case maybeUser of
        Nothing -> do
            putStrLn ("Couldn't find matching user for user name: " ++ show u)
            return ()
        Just user -> sendAll soc (formatUser user)

returnUsers :: Socket -> SQLite.Connection -> IO ()
returnUsers soc db = do
    rows <- SQLite.query_ db allUsers
    let userNames = map userName rows
        newLineSeparated = T.concat $ intersperse "\n" userNames
    sendAll soc (encodeUtf8 newLineSeparated)

handleQuery :: Socket -> SQLite.Connection -> IO ()
handleQuery soc db = do
    message <- recv soc maxMessageLength
    print message
    case message of
        "\r\n" -> returnUsers soc db
        name -> returnUser soc db (decodeUtf8 name)

handleQueries :: Socket -> SQLite.Connection -> IO ()
handleQueries sock db = forever $ do
    (soc, _) <- accept sock
    putStrLn "Got connection, handling query"
    handleQuery soc db
    sClose soc

withClientSocket :: Family -> (Socket -> IO a) -> IO a
withClientSocket family =
    bracket
        (socket family Stream defaultProtocol)
        sClose

runServer :: IO ()
runServer = withSocketsDo $ do
    addrInfos <- getAddrInfo
        (Just (defaultHints { addrFlags = [ AI_PASSIVE ] }))
        Nothing
        (Just $ show fingerPort)
    let addrInfo = head addrInfos
        address = addrAddress addrInfo
    withClientSocket (addrFamily addrInfo) $ \sock -> do
        bindSocket sock address
        listen sock 1
        SQLite.withConnection dbFileName (handleQueries sock)

main :: IO ()
main = do
    args <- getArgs
    let arg = head args
    case arg of
        "init" -> createDatabase
        "run" -> runServer
