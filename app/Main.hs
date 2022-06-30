{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text, pack, append)
import           Control.Applicative              ((<|>))
import qualified Telegram.Bot.API          as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser
import           Telegram.Bot.API.Types 
import           Telegram.Bot.Simple.Conversation
import           Telegram.Bot.API.GettingUpdates
import Prelude

-- | import project modules

-- import text for messages
import Message.TextCreator

-- import ranks for haskeller
import Ranking (findNewRank)

-- import database stuff
import qualified DB.Utils as DB

-- | type aliases to semantically show 
-- where necessary info is stored  
type Size = Int
type Name = Text
type RankName = Text

-- | function that shows how to distinguish 'conversations'
-- ('Conversations' are distinguished by chat id)
updateToConversation :: Telegram.Update -> Maybe ChatId
updateToConversation update = chatIdInt
    where
        chatIdInt = case (updateMessage update) of
            Nothing -> Nothing
            (Just message) -> Just $ chatId $ messageChat message
                    

-- | Bot model: HaskellerState(size, name, rank)
data Model = Model Size Name RankName
    deriving (Show)

-- | Actions bot can perform.
data Action
  = NoAction                        -- ^ Perform no action.
  | ShowStatus                      -- ^ Action to show all info available about the haskeller
  | Grow                            -- ^ Action to increase IQ of haskeller
  | ChangeName Text                 -- ^ Action to changing name of the haskeller
  | Rank                            -- ^ Action to show rank of the haskeller
  | NewRankNotification RankName    -- ^ Action to show notification about new Rank
  deriving (Show)


-- | Bot application.
bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = (Model 0 "" "Newbie")
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

-- | bot for several 'conversations'
sevBot = conversationBot updateToConversation bot


-- | Text to Action
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate(
    ChangeName <$> command "change_name" <|>
    Grow <$ command "grow" <|>
    Rank <$ command "rank" <|>
    ShowStatus <$ command "status")

-- | Action&Model to programm of Action' Model'
handleAction :: Action -> Model -> Eff Action Model
handleAction action model@(Model size name rank) = case action of

    NoAction -> pure model -- nothing to do

    ChangeName newName -> (Model size newName rank) <# do -- change name
        replyText (changeNameMessageText name newName)
        pure NoAction

    Grow -> (Model (size + 1) name rank) <# do -- increases IQ by 1
        replyText (growMessageText name (pack (show (size + 1))))

        -- If new rank is reached, notifies about it and change it
        case findNewRank (size + 1) of
            Nothing -> pure NoAction
            (Just newRank) -> pure (NewRankNotification newRank)

    ShowStatus -> model <# do -- shows all available information about haskeller
        replyText (append (pack (show(size + 1) ++ " ")) name)
        pure Rank -- to show rank

    NewRankNotification newRank -> (Model size name newRank) <# do -- notifies user about new rank
        -- + changes new rank
        replyText "New Rank!!!"
        pure NoAction

    Rank -> model <# do -- shows rank of the haskeller
        replyText rank
        pure NoAction


-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault sevBot) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
main :: IO ()
main = do
    DB.dropHTable
    DB.createHTable
    DB.addHaskeller 1 "name" 0 "rank" "time123"
    DB.printAll
    DB.updateName 1 "newName"
    DB.updateIQ 1 10
    DB.updateRank 1 "newRank"
    DB.printAll

    putStrLn "Please enter telegram token:"
    tgToken <- getLine

    run (Telegram.Token (pack tgToken))
