module DB where

-- dep
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

-- test data (compare with table)
data Test = Test 
    { id
    , str
    }
instance Show Test where
    show test = mconcat 
        [ show $ id test
        , ", "
        , str test
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

-- exec in command line
-- sqlite3 db.db < CreateTable.sql
createTable = undefined

test :: IO()
test = putStrLn "Hello, world"