{-# LANGUAGE OverloadedStrings #-}
module DB.Connection where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow


withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
    conn <- open dbName
    action conn
    close conn
withDBConn = withConn "db.db"