{-# LANGUAGE OverloadedStrings #-}
module Ranking where

import Data.Text

-- | all ranks, now stored inside code, because they are immutable
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

-- | function that returns rank if new rank is reached
-- otherwise - returns Nothing

findNewRank :: Int -> Maybe Text
findNewRank size = lookup' size ranking
