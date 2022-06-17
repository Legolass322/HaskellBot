{-# LANGUAGE OverloadedStrings #-}
module Message.TextCreator where 

import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid

-- >>> changeNameMessageText "someOldName" "newCoolName"
-- "Now your someOldName is newCoolName!"
changeNameMessageText :: Text -> Text -> Text
changeNameMessageText oldName newName = mconcat ["Now your ", oldName, " is ", newName, "!"]

-- >>> growMessageText "Ivan" "3"
-- "Excellent! Now your Ivan is smarter! It's now has 3 IQ"
growMessageText :: Text -> Text -> Text
growMessageText name size = mconcat ["Excellent! Now your ", name, " is smarter! It's now has ", size, " IQ"]

-- >>> rankMessageText "Ivan" "Haskell Bot Developer"
-- "Your Ivan has Haskell Bot Developer rank"
rankMessageText :: Text -> Text -> Text
rankMessageText name rank = mconcat ["Your ", name, " has ", rank, " rank"]

-- >>> stateMessageText "Ivan" "3" "Haskell Bot Developer"
-- "Your Ivan has 3 IQ (rank: Haskell Bot Developer) "
statusMessageText :: Text -> Text -> Text -> Text 
statusMessageText name size rank = mconcat ["Your ", name, " has ", size, " IQ ", "(rank: ", rank, ") "] 
