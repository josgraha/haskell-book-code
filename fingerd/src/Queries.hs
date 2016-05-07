{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Queries (allUsers, createUsers, fetchUser, insertUser) where

import qualified Database.SQLite.Simple as SQLite
import           Text.RawString.QQ

fetchUser :: SQLite.Query
fetchUser = "SELECT * FROM users WHERE userName = ?"

allUsers :: SQLite.Query
allUsers = "SELECT * FROM users"

createUsers :: SQLite.Query
createUsers = [r|
    CREATE TABLE IF NOT EXISTS users
    ( id INTEGER PRIMARY KEY AUTOINCREMENT
    , userName TEXT UNIQUE
    , shell TEXT
    , homeDir TEXT
    , fullName TEXT
    , phone TEXT
    )
|]

insertUser :: SQLite.Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"
