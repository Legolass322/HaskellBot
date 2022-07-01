{-# LANGUAGE OverloadedStrings #-}
module DB.Models where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow


data Haskeller = Haskeller 
    { chatId :: Int
    , name :: String
    , iq :: Int
    , rank :: String
    , time :: String -- fix
    }
instance Show Haskeller where
    show haskr = mconcat
        [ show $ "Haskr: " 
        , show (chatId haskr)
        , " - name: "
        , name haskr
        , ", iq: "
        , show (iq haskr)
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