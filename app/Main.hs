{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import project modules
import           Control.Applicative            ( (<|>) )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Text                      ( Text
                                                , append
                                                , pack
                                                )
import           Data.Time
-- import text for messages
import           Message.TextCreator
import           Prelude
-- import ranks for haskeller
import           Ranking                        ( findNewRank )
import qualified Telegram.Bot.API              as Telegram
import           Telegram.Bot.API.GettingUpdates
import           Telegram.Bot.API.Types
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Conversation
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser
import           TimeApi

{- | type aliases to semantically show
 where necessary info is stored
-}
type Size = Int

type Name = Text

type RankName = Text

type LastGrowth = UTCTime

type Flag = Bool

-- | Cooldown of growth in seconds
cooldown = 10

{- | function that shows how to distinguish 'conversations'
 ('Conversations' are distinguished by chat id)
-}
updateToConversation :: Telegram.Update -> Maybe ChatId
updateToConversation update = chatIdInt
  where
    chatIdInt = case updateMessage update of
        Nothing        -> Nothing
        (Just message) -> Just $ chatId $ messageChat message

-- | Bot conversation state model.
data Model = Model Size Name RankName LastGrowth Flag
    deriving Show

-- | Actions bot can perform.
data Action
  = NoAction                               -- ^ Perform no action.
  | ShowInfo                               -- ^ Action to show all info available about the haskeller
  | GrowCommand                            -- ^ Action to increase IQ of haskeller
  | NotifyThatCannotGrow NominalDiffTime   -- ^ ?
  | Grow UTCTime                           -- ^ ?
  | ChangeName                             -- ^ Action to change the flag and pure InputName
  | NewRankNotification RankName           -- ^ Action to show notification about new Rank
  | Start                                  -- ^ Display start message
  | InputName Text                         -- ^ Action to change name of haskeller 
  deriving (Show)

-- | Bot application.
bot :: UTCTime -> BotApp Model Action
bot time = BotApp { botInitialModel = Model 0 "Haskeller" "Newbie" time False
                  , botAction       = flip handleUpdate
                  , botHandler      = handleAction
                  , botJobs         = []
                  }

-- | bot for several 'conversations'
sevBot time = conversationBot updateToConversation (bot time)

{- | How to process incoming 'Telegram.Update's
 and turn them into 'Action's.
-}
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ =
    parseUpdate
        $   ChangeName <$  command "change_name"
        <|> GrowCommand <$  command "grow"
        <|> ShowInfo <$  command "info"
        <|> Start <$  command "start"
        <|> InputName <$> text

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction action model@(Model size name rank time flag) = case action of
    NoAction -> pure model -- nothing to do

    Start    -> model <# do -- to start
        reply (toReplyMessage startMessageText)
            { replyMessageReplyMarkup = Just
                $ Telegram.SomeReplyKeyboardMarkup startMessageKeyboard
            }
        pure NoAction

    ChangeName -> Model size name rank time True <# do -- change the flag & pure InputName 
        replyText enterNewNameText
        pure NoAction

    GrowCommand ->  -- ?
        model <# do
            currentTime <- liftIO getCurrentTime
            if checkForGrowth time currentTime cooldown
            then
                pure (Grow currentTime) 
            else 
                pure (NotifyThatCannotGrow (abs(diffUTCTime time currentTime)))

    InputName newName -> if flag -- change name if flag, else pure NoAction
        then Model size newName rank time flag <# do
            replyText (changeNameMessageText name newName)
            pure NoAction
        else model <# pure NoAction

    ShowInfo -> model <# do -- shows all available information about haskeller
        replyText (statusMessageText name (pack (show size)) rank)
        pure NoAction

    NewRankNotification newRank -> Model size name newRank time flag <# do -- notifies user about new rank and changes new rank
        replyText (newRankMessageText name newRank)
        pure NoAction
    
    Grow newTime -> Model (size + 1) name rank newTime flag <# do -- increases IQ by 1
        replyText (growMessageText name (pack (show (size + 1)))) -- If new rank is reached, notifies about it and change it
        case findNewRank (size + 1) of
            Nothing        -> pure NoAction
            (Just newRank) -> pure (NewRankNotification newRank)

    NotifyThatCannotGrow deltaTime -> model <# do -- ?
        replyText (cannotGrowMessageText name)
        pure NoAction

-- | A keyboard with actions
startMessageKeyboard :: Telegram.ReplyKeyboardMarkup
startMessageKeyboard = Telegram.ReplyKeyboardMarkup
    { Telegram.replyKeyboardMarkupKeyboard = [ ["/grow", "/info" ,"/change_name"]
                                             ]
    , Telegram.replyKeyboardMarkupResizeKeyboard     = Just True
    , Telegram.replyKeyboardMarkupOneTimeKeyboard    = Just False
    , Telegram.replyKeyboardMarkupSelective          = Nothing
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
    tgToken     <- getLine
    currentTime <- getCurrentTime
    run (Telegram.Token (pack tgToken)) currentTime
