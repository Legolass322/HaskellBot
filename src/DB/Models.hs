{-# LANGUAGE OverloadedStrings #-}
module DB.Models where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Text


data Haskeller = Haskeller 
    { chatId :: Int
    , name :: Text
    , iq :: Int
    , rank :: Text
    , time :: Text -- fix
    }
instance Show Haskeller where
    show haskr = unpack $ mconcat
        [  "Haskr: " 
        , pack $ show (chatId haskr)
        , " - name: "
        , name haskr
        , ", iq: "
        , pack $ show (iq haskr)
        , ", rank: "
        , rank haskr
        , ", time: "
        , time haskr
        ]
instance FromRow Haskeller where
    fromRow = Haskeller 
        <$> field
        <*> field
        <*> field
        <*> field
        <*> field