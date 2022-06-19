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
import           Ranking
import System.Environment

-- | Code sources
import Message.TextCreator
import Ranking (findNewRank)

-- | Type macros
-- * Model types
type Size = Int
type Name = Text
type RankName = Text

-- | Dealing with several users
updateToConversation :: Telegram.Update -> Maybe ChatId
updateToConversation update = chatIdInt
    where
        chatIdInt = case updateMessage update of
            Nothing -> Nothing
            (Just message) -> Just $ chatId $ messageChat message


-- | Bot model: HaskellerState(size, name, rank)
data Model = Model Size Name RankName
    deriving (Show)

-- | Actions bot can perform.
data Action
  = NoAction
  | ShowStatus
  | Grow
  | ChangeName Text
  | Rank
  | NewRankNotification RankName
  deriving (Show)


-- | Bot application bindings init
-- @param botInitialModel [Model]
-- @param botAction [(tgupdate->model->action)] action to text binding
-- @param botHandler [(action->model->action.model)] action&model to program(action' model')
-- @param botJobs ???
bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model 0 "" "defaultRank"
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

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
    NoAction -> pure model
    ChangeName newName -> (Model size newName rank) <# do
        replyText (changeNameMessageText name newName)
        pure NoAction
    Grow -> (Model (size + 1) name rank) <# do
        replyText (growMessageText name (pack (show (size + 1))))
        case findNewRank (size + 1) of
            Nothing -> pure NoAction
            (Just newRank) -> pure (NewRankNotification newRank)
    ShowStatus -> model <# do
        replyText (statusMessageText name (pack (show(size + 1))) rank)
        pure Rank
    NewRankNotification newRank -> (Model size name newRank) <# do
        replyText "New Rank!!!"
        pure NoAction
    Rank -> model <# do
        replyText (rankMessageText name rank)
        pure NoAction

-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault sevBot) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
main :: IO ()
main = do
    setEnv "TELEGRAM_BOT_TOKEN" "PUT_TOKEN_HERE"
    getEnvToken "TELEGRAM_BOT_TOKEN" >>= run