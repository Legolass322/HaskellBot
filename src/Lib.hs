module Lib where
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

test :: IO ()
test = do
  conn <- open "../db/test.db"
  execute conn "INSERT INTO test (str) VALUES (?)"
    (Only ("test string 2" :: Query))
  r <- query_ conn "SELECT * FROM test" :: IO [TestField]
  mapM_ print r
  close conn