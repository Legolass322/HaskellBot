{-# LANGUAGE OverloadedStrings #-}
module DB.TableCreator where

-- dep
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

-- test data (compare with table)
data Test = Test 
    { idTest :: Int
    , strTest :: String
    }

instance Show Test where
    show test = mconcat 
        [ show $ idTest test
        , ", "
        , strTest test
        ]
instance FromRow Test where
    fromRow = Test 
        <$> field 
        <*> field
-- test of functionality

-- Appling action to db file
withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
    conn <- open dbName
    action conn
    close conn 

-- Pushing string to db
addTest :: String -> IO ()
addTest str = withConn "db.db" $ 
    \conn -> do
        execute conn "INSERT INTO test (str) VALUES (?);" (Only str)
        print "string added"

-- Printing all strings from db
printTest :: IO ()
printTest = withConn "db.db" $
    \conn -> do
        rows <- query_ conn "SELECT * FROM test;" :: IO [Test]
        mapM_ print rows

tryCreateDBWithTable :: IO()
tryCreateDBWithTable = withConn "db.db" $
    \conn -> do
        execute_ conn "DROP TABLE IF EXISTS test;"
        execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY,str TEXT);"
        print "table created"

-- exec in command line
-- sqlite3 db.db < CreateTable.sql
createTable = undefined

test :: IO()
test = do 
    tryCreateDBWithTable
    addTest "string1"
    addTest "string2"
    printTest