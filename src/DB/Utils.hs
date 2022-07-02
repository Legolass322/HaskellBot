{-# LANGUAGE OverloadedStrings #-}
module DB.Utils where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time
import Data.Text

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
   

updateName  :: Int      -- chatId
            -> Text   -- Name
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
            -> Text   -- rank
            -> IO ()
updateRank chatId rank = withDBConn $
    \conn -> do
        execute conn 
            "UPDATE haskellers SET rank = ? WHERE chatId = ?"
            (rank, chatId)


updateTime  :: Int      -- chatId
            -> UTCTime  -- time
            -> IO ()
updateTime chatId time = withDBConn $
    \conn -> do
        execute conn 
            "UPDATE haskellers SET time = ? WHERE chatId = ?"
            (show time, chatId)

addHaskeller    :: Int      -- chatID
                -> Text   -- name
                -> Int      -- iq
                -> Text   -- rank
                -> UTCTime  -- time
                -> IO ()
addHaskeller chatId name iq rank time = withDBConn $
    \conn -> do
        execute conn 
            "INSERT INTO haskellers (chatId, name, iq, rank, time) VALUES (?, ?, ?, ?, ?)" 
            (chatId, name, iq, rank, show time)
        print "Haskeller pushed"

printAll = withDBConn $
    \conn -> do
        rows <- query_ conn "SELECT * FROM haskellers" :: IO [Haskeller]
        mapM_ print rows

getByChatId :: Int -> (Haskeller -> IO ()) -> IO ()
getByChatId chatId action = withDBConn $
    \conn -> do
        haskeller <- query conn 
            "SELECT * FROM haskellers WHERE chatId = ? LIMIT 1"
            (Only chatId)
        mapM_ action haskeller

getAll :: IO [Haskeller]
getAll = withDBConn $
    \conn -> do
        rows <- query_ conn "SELECT * FROM haskellers" :: IO [Haskeller]
        return rows

getTop  :: Int -- Amount
        -> IO [Haskeller]
getTop amount = withDBConn $
    \conn -> do
        rows <- query conn 
            "SELECT * FROM haskellers ORDER BY iq LIMIT ?"
            (Only amount)
        return rows
        