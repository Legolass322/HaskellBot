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
import System.Environment
import Message.TextCreator
import Rankings


type Size = Int
type Name = Text
type RankName = Text

updateToConversation :: Telegram.Update -> Maybe ChatId
updateToConversation update = chatIdInt
    where
        chatIdInt = case (updateMessage update) of
            Nothing -> Nothing
            (Just message) -> Just $ chatId $ messageChat message
                    

-- | Bot conversation state model.
data Model = Model Size Name RankName
    deriving (Show)

-- | Actions bot can perform.
data Action
  = NoAction    -- ^ Perform no action.
  | ShowStatus
  | Grow
  | Name Text
  | Rank
  deriving (Show)


-- | Bot application.
bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = (Model 0 "" "defaultRank")
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

sevBot = conversationBot updateToConversation bot


-- | How to process incoming 'Telegram.Update's
-- and turn them into 'Action's.
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate(
    Name <$> command "name" <|>
    Grow <$ command "grow" <|>
    Rank <$ command "rank" <|>
    ShowStatus <$ command "status")

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction action model@(Model size name rank) = case action of
    NoAction -> pure model
    Name newName -> (Model size newName rank) <# do
        replyText (changeNameMessageText name newName)
        pure ShowStatus
    Grow -> (Model (size + 1) name rank) <# do
        replyText (growMessageText name (pack (show (size + 1))))
        pure ShowStatus
    ShowStatus -> model <# do
        replyText (append (pack (show(size + 1) ++ " ")) name)
        pure Rank
    Rank -> model <# do
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
    setEnv "TELEGRAM_BOT_TOKEN" "YOUR TELEGRAM BOT TOKEN"
    getEnvToken "TELEGRAM_BOT_TOKEN" >>= run