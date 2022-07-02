{-# LANGUAGE OverloadedStrings #-}
module DB.Connection where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow


withConn :: String -> (Connection -> IO a) -> IO a
withConn dbName action = do
    conn <- open dbName
    result <- action conn
    close conn
    return result
withDBConn = withConn "db.db"