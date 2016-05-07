module User (User(..), UserRow) where

import           Data.Text
import           Database.SQLite.Simple
import           Database.SQLite.Simple.Types

type UserRow = (Null, Text, Text, Text, Text, Text)

data User = User
    { userId :: Int
    , userName :: Text
    , shell :: Text
    , homeDir :: Text
    , fullName :: Text
    , phone :: Text
    } deriving (Eq, Show)

instance FromRow User where
    fromRow = User
        <$> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
