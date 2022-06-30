{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text, pack, append)
import Data.Time
import           Control.Applicative              ((<|>))
import qualified Telegram.Bot.API          as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser
import           Telegram.Bot.API.Types
import           Telegram.Bot.Simple.Conversation
import           Telegram.Bot.API.GettingUpdates
import           Control.Monad.IO.Class (liftIO)
import Prelude

-- | import project modules

-- import text for messages
import Message.TextCreator

-- import ranks for haskeller
import Ranking (findNewRank)
import TimeApi


-- | type aliases to semantically show 
-- where necessary info is stored  
type Size = Int
type Name = Text
type RankName = Text
type LastGrowth = UTCTime

-- | Cooldown of growth in seconds
cooldown = 10

-- | function that shows how to distinguish 'conversations'
-- ('Conversations' are distinguished by chat id)
updateToConversation :: Telegram.Update -> Maybe ChatId
updateToConversation update = chatIdInt
    where
        chatIdInt = case updateMessage update of
            Nothing -> Nothing
            (Just message) -> Just $ chatId $ messageChat message


-- | Bot conversation state model.
data Model = Model Size Name RankName LastGrowth 
    deriving (Show)

-- | Actions bot can perform.
data Action
  = NoAction                        -- ^ Perform no action.
  | ShowStatus                      -- ^ Action to show all info available about the haskeller  
  | Grow                            -- ^ Action to grow haskeller by 1 IQ
  | Start                           -- ^ Action to start & open keyboard
  | ChangeName Text                 -- ^ Action to changing name of the haskeller
  | Rank                            -- ^ Action to show rank of the haskeller
  | NewRankNotification RankName    -- ^ Action to show notification about new Rank
  deriving (Show)

-- | Bot application.
bot :: UTCTime -> BotApp Model Action
bot time = BotApp
  { botInitialModel = Model 0 "" "Newbie" time
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

-- | bot for several 'conversations'
sevBot time = conversationBot updateToConversation (bot time)

-- | How to process incoming 'Telegram.Update's
-- and turn them into 'Action's.
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate $
    ChangeName <$> command "change_name" <|>
    Grow <$ command "grow" <|>
    Rank <$ command "rank" <|>
    ShowStatus <$ command "status" <|>
    Start <$ command "start" 

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction action model@(Model size name rank time) = case action of

    NoAction -> pure model -- nothing to do

    Start -> model <# do -- to start
                reply (toReplyMessage startMessageText)
                    { replyMessageReplyMarkup = Just $
                        Telegram.SomeReplyKeyboardMarkup startMessageKeyboard
                    }
                pure NoAction

    ChangeName newName -> Model size newName rank time <# do -- change name
        replyText (changeNameMessageText name newName)
        pure NoAction

    ShowStatus -> model <# do -- shows all available information about haskeller
        replyText (statusMessageText name (pack (show size)) rank)
        pure NoAction

    NewRankNotification newRank -> model <# do -- notifies user about new rank
        -- + changes new rank
        replyText (newRankMessageText name newRank)
        pure NoAction

    Rank -> model <# do -- shows rank of the haskeller
        replyText (rankMessageText name rank)
        pure NoAction

    Grow -> Model (size + 1) name rank time <# do -- increases IQ by 1
                replyText (growMessageText name (pack (show (size + 1))))

                -- If new rank is reached, notifies about it and change it
                case findNewRank (size + 1) of
                    Nothing -> pure NoAction
                    (Just newRank) -> pure (NewRankNotification newRank)

startMessageKeyboard :: Telegram.ReplyKeyboardMarkup
startMessageKeyboard = Telegram.ReplyKeyboardMarkup
    { Telegram.replyKeyboardMarkupKeyboard =
        [ [ "/grow", "/change_name" ] , [ "/status", "/rank" ] ]
    , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
    , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just True
    , Telegram.replyKeyboardMarkupSelective = Nothing
    , Telegram.replyKeyboardMarkupInputFieldSelector = Nothing
    }

-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> UTCTime -> IO ()
run token time = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault (sevBot time)) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
main :: IO ()
main = do
    putStrLn "Please enter telegram token:"
    tgToken <- getLine

    currentTime <- getCurrentTime

    run (Telegram.Token (pack tgToken)) currentTime