{-# LANGUAGE OverloadedStrings #-}
module Ranking where

import Data.Text

ranking = 
  [  (10, "Telegram Bot Freelancer")
  ,  (20, "Builder")
  ,  (30, "Cashier")
  ,  (40, "SMM Specialist")
  ,  (50, "Tester")
  ,  (60, "Python Developer")
  ,  (70, "Web Designer")  
  ,  (80, "Java Developer")  
  ,  (90, "Software Developer")  
  ,  (100, "Haskeller")  ]


lookup' :: Int -> [(Int, Text)] -> Maybe Text
lookup' _ [] =  Nothing
lookup' key ((x, y) : xys)
  | key == x  =  Just y
  | otherwise =  lookup' key xys
