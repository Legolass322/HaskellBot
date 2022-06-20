{-# LANGUAGE OverloadedStrings #-}
module Message.TextCreator where 

import Data.Text (Text)
import qualified Data.Text as T

-- >>> changeNameMessageText "someOldName" "newCoolName"
-- "Now your someOldName is newCoolName!"
changeNameMessageText :: Text -> Text -> Text
changeNameMessageText oldName newName = mconcat ["Now your ", oldName, " is ", newName, "!"]

-- >>> growMessageText "Ivan" "3"
-- "Excellent! Now your Ivan is smarter! It's now has 3 IQ"
growMessageText :: Text -> Text -> Text
growMessageText name size' = mconcat ["Excellent! Now your ", name, " is smarter! It's now has ", size', " IQ"]
