{-# LANGUAGE OverloadedStrings #-}
module Message.TextCreator where 

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text


-- >>> changeNameMessageText "someOldName" "newCoolName"
-- Now your someOldName is newCoolName!
changeNameMessageText :: Text -> Text -> Text
changeNameMessageText oldName newName = mconcat ["\x26A1 Now your \x1F9B9 ", oldName, " is ", newName, "!"]

-- >>> growMessageText "Ivan" "3"
-- Excellent! Now your Ivan is smarter!
-- 
-- Your haskeller has 3 IQ!
growMessageText :: Text -> Text -> Text
growMessageText name size = mconcat ["\x2728 Excellent! Now your \x1F9B9 ", name, " is smarter \x1F60E ! \n \nYour haskeller has ", size, " IQ!"]

-- >>> newRankMessageText "Ivan" "Haskell Bot Developer"
-- New Rank!
-- Your Ivan has Haskell Bot Developer rank
newRankMessageText :: Text -> Text -> Text
newRankMessageText name rank =  Text.unlines ["\x1F389 New Rank!", rankText]
  where rankText = rankMessageText name rank

-- >>> rankMessageText "Ivan" "Haskell Bot Developer"
-- Your Ivan has Haskell Bot Developer rank
rankMessageText :: Text -> Text -> Text
rankMessageText name rank = mconcat ["\x1F525 Your ", name, " has ", rank, " rank"]

-- >>> statusMessageText "Ivan" "3" "Haskell Bot Developer"
-- Your Ivan has 3 IQ (rank: Haskell Bot Developer)
statusMessageText :: Text -> Text -> Text -> Text 
statusMessageText name size rank = mconcat ["\x26A0 Your ", name, " has ", size, " IQ ", "(rank: ", rank, ") "] 

-- >>> startMessageText
-- Hi there! I am your Haskeller bot 
-- 
-- I can help you grow your own Haskeller!:
-- 
-- - Use /change_name to change the name of your Haskeller
-- - Use /grow to grow by 1 IQ of your Haskeller and see the current IQ
-- - Use /rank to see the current rank (each 10 IQ, the rank changes to the new one)
-- - Use /status to see all the characteristics of your Haskeller (name, IQ, rank)
--
--
-- Enjoy!

startMessageText :: Text
startMessageText = Text.unlines
 [ "\x1F596 Hi there! I am your Haskeller bot "
 , ""
 , "I can help you grow your own Haskeller!:"
 , ""
 , "- Use /change_name to change the name of your Haskeller"
 , "- Use /grow to grow by 1 IQ of your Haskeller and see the current IQ"
 , "- Use /rank to see the current rank (each 10 IQ, the rank changes to the new one)"
 , "- Use /status to see all the characteristics of your Haskeller (name, IQ, rank)"
 , ""
 , ""
 , "Enjoy!"
 ]
