{-# LANGUAGE OverloadedStrings #-}
module DB.Utils where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Text (Text, pack)

import DB.Connection (withDBConn)
import DB.Models (Haskeller)

dropHTable :: IO ()
dropHTable = withDBConn $
    \conn -> do 
        execute_ conn $
            "DROP TABLE IF EXISTS haskellers"
        print "Table haskellers dropped"

createHTable :: IO()
createHTable = withDBConn $
    \conn -> do
        execute_ conn $
            "CREATE TABLE IF NOT EXISTS haskellers (chatId INTEGER PRIMARY KEY, name TEXT, iq INTEGER, rank TEXT, time TEXT)"
        print "Table haskellers created"

-- updateFieldByChatId :: (Show a, Show b) => String -> a -> b -> IO ()
-- updateFieldByChatId fieldName field chatId = withDBConn $
--     \conn -> do
--         execute conn updateQuery (show field, chatId)
--         print $ concat 
--             [ "Field ", fieldName
--             , " in ", (show chatId)
--             , " updated to " , (show field)]
--     where
--         updateQuery = case fieldName of
--             "name" -> 
--                 "UPDATE haskellers SET name = ? WHERE chatId = ?" 
--             "iq" ->
--                 "UPDATE haskellers SET iq = ? WHERE chatId = ?" 
--             "rank" ->
--                 "UPDATE haskellers SET rank = ? WHERE chatId = ?" 
--             "time" ->
--                 "UPDATE haskellers SET time = ? WHERE chatId = ?" 
        


updateName  :: Int      -- chatId
            -> String   -- Name
            -> IO ()
updateName chatId name = withDBConn $
    \conn -> do
        execute conn 
            "UPDATE haskellers SET name = ? WHERE chatId = ?"
            (name, chatId)

updateIQ    :: Int      -- chatId
            -> Int      -- IQ
            -> IO ()
updateIQ chatId iq = withDBConn $
    \conn -> do
        execute conn 
            "UPDATE haskellers SET iq = ? WHERE chatId = ?"
            (iq, chatId)


updateRank  :: Int      -- chatId
            -> String   -- rank
            -> IO ()
updateRank chatId rank = withDBConn $
    \conn -> do
        execute conn 
            "UPDATE haskellers SET rank = ? WHERE chatId = ?"
            (rank, chatId)


updateTime  :: Int      -- chatId
            -> String   -- time
            -> IO ()
updateTime chatId time = withDBConn $
    \conn -> do
        execute conn 
            "UPDATE haskellers SET time = ? WHERE chatId = ?"
            (time, chatId)

addHaskeller    :: Int      -- chatID
                -> String   -- name
                -> Int      -- iq
                -> String   -- rank
                -> String   -- time
                -> IO ()
addHaskeller chatId name iq rank time = withDBConn $
    \conn -> do
        execute conn 
            "INSERT INTO haskellers (chatId, name, iq, rank, time) VALUES (?, ?, ?, ?, ?)" 
            (chatId, name, iq, rank, time)
        print "Haskeller pushed"

printAll = withDBConn $
    \conn -> do
        rows <- query_ conn "SELECT * FROM haskellers" :: IO [Haskeller]
        mapM_ print rows