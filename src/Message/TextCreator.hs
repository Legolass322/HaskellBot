{-# LANGUAGE OverloadedStrings #-}
module Message.TextCreator where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time                      ( NominalDiffTime )

-- >>> enterNewNameText
-- Please enter new name:
enterNewNameText :: Text
enterNewNameText = "Please enter new name:"

-- >>> changeNameMessageText "someOldName" "newCoolName"
-- Now your someOldName is newCoolName!
changeNameMessageText :: Text -> Text -> Text
changeNameMessageText oldName newName =
    mconcat ["\x26A1 Now your ", oldName, " is ", newName, "!"]

-- >>> growMessageText "Ivan" "3"
-- Excellent! Now your Ivan is smarter!
-- 
-- Your haskeller has 3 IQ!
growMessageText :: Text -> Text -> Text
growMessageText name size = mconcat
    [ "\x2728 Excellent! Now your "
    , name
    , " is smarter! \n \n"
    , name
    , " has "
    , size
    , " IQ!"
    ]

-- >>> cannotGrowMessageText
-- Please wait, your Haskeler still growing
cannotGrowMessageText :: Text -> NominalDiffTime -> Text
cannotGrowMessageText name time = mconcat
    [ "\x1F558 Please wait "
    , T.pack (show time)
    , "ec more, your "
    , name
    , " still growing"
    ]

-- >>> newRankMessageText "Ivan" "Haskell Bot Developer"
-- New Rank!
-- Your Ivan has Haskell Bot Developer rank
newRankMessageText :: Text -> Text -> Text
newRankMessageText name rank = T.unlines
    [ "\x1F389 New Rank!"
    , mconcat ["\x1F525 Your ", name, " has ", rank, " rank"]
    ]

-- >>> statusMessageText "Ivan" "3" "Haskell Bot Developer"
-- Your Ivan has 3 IQ (rank: Haskell Bot Developer)
statusMessageText :: Text -> Text -> Text -> Text
statusMessageText name size rank =
    mconcat ["\x26A0 Your ", name, " has ", size, " IQ ", "(rank: ", rank, ") "]

-- >>> startMessageText
-- Hi there! I am your Haskeller bot 
-- 
-- I can help you grow your own Haskeller!:
-- 
-- - Use /change_name to change the name of your Haskeller
-- - Use /grow to grow by 1 IQ of your Haskeller and see the current IQ
-- - Use /info to see all the characteristics of your Haskeller (name, IQ, rank*)
--
-- *Each 10 IQ the rank changes to the new one
--
-- The process of growing your haskelist takes some time
-- Wait 10 seconds after /grow command to grow again!
--
-- Enjoy!

startMessageText :: Text
startMessageText = T.unlines
    [ "\x1F596 Hi there! I am your Haskeller bot "
    , ""
    , "I can help you grow your own Haskeller!:"
    , ""
    , "- Use /change_name to change the name of your Haskeller"
    , "- Use /grow to grow by 1 IQ of your Haskeller and see the current IQ"
    , "- Use /info to see all the characteristics of your Haskeller (name, IQ, rank*)"
    , ""
    , "*The rank changes to the new one each 10 IQ"
    , ""
    , "\x1F558 The process of growing your haskelist takes some time"
    , "Wait 10 seconds after /grow command to grow again!"
    , ""
    , "Enjoy!"
    ]


-- >>> leaderBoardMessageText [("Ilnur", 10), ("Diana", 9), ("Dima", 6), ("Albert", 2)]
-- "1. Ilnur 10\n2. Diana 9\n3. Dima 6\n4. Albert 2\n"

leaderBoardMessageText :: [(Text, Int)] -> Text
leaderBoardMessageText board = T.unlines
    (zipWith
        (<>)
        (map (\n -> T.pack (show n) <> ". ") [1 ..])
        (map (\(name, size) -> name <> " " <> T.pack (show size)) board)
    )

