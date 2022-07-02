{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import project modules
import           Control.Applicative            ( (<|>) )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Text                      ( Text
                                                , append
                                                , pack, unpack
                                                )
import           Data.Time
import           Data.HashMap.Strict hiding (map)
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

-- import database stuff
import qualified DB.Utils as DB
import qualified DB.Models as DBModels

{- | type aliases to semantically show
 where necessary info is stored
-}
type Size = Int

type Name = Text

type RankName = Text

type LastGrowth = UTCTime

type ChangeNameFlag = Bool

-- | Cooldown of growth in seconds
cooldown :: Num a => a
cooldown = 10

{- | function that shows how to distinguish 'conversations'
 ('Conversations' are distinguished by chat id)
-}
updateToConversation :: Telegram.Update -> Maybe ChatId
updateToConversation = chatIdInt

chatIdInt update = case updateMessage update of
        Nothing        -> Nothing
        (Just message) -> Just $ chatId $ messageChat message

-- | Bot conversation state model.
data Model = Model Size Name RankName LastGrowth ChangeNameFlag 
    deriving Show

-- | Actions bot can perform.
data Action
  = NoAction                               -- ^ Perform no action.
  | ShowInfo                               -- ^ Action to show all info available about the haskeller
  | GrowCommand (Maybe ChatId)                            -- ^ Action to increase IQ of haskeller
  | NotifyThatCannotGrow NominalDiffTime   -- ^ ?
  | Grow (Maybe ChatId) UTCTime                           -- ^ ?
  | ChangeName                             -- ^ Action to change the flag and pure InputName
  | NewRankNotification (Maybe ChatId) RankName           -- ^ Action to show notification about new Rank
  | Start (Maybe ChatId)                                  -- ^ Display start message
  | InputName (Maybe ChatId) Text                         -- ^ Action to change name of haskeller 
  deriving (Show)

-- | Bot application.
bot :: UTCTime -> BotApp Model Action

bot time = BotApp { botInitialModel = Model 0 "Haskeller" "Newbie" time False
                  , botAction       = flip handleUpdate
                  , botHandler      = handleAction
                  , botJobs         = []
                  }

-- | bot for several 'conversations'
sevBot time haskellers = BotApp {   
    botInitialModel = hashmapOfHaskellers,
    botAction = botAction intermediateBot,
    botHandler = botHandler intermediateBot,
    botJobs = botJobs intermediateBot
}
    where
        intermediateBot = conversationBot updateToConversation (bot time)

        haskellerToHashmap haskeller = (Just $ ChatId $ toInteger (DBModels.chatId haskeller), Model (DBModels.iq haskeller) (DBModels.name haskeller) (DBModels.rank haskeller) (read $ unpack $ DBModels.time haskeller) False)
        haskellersToHashmap = map haskellerToHashmap haskellers

        hashmapOfHaskellers = fromList haskellersToHashmap

{- | How to process incoming 'Telegram.Update's
 and turn them into 'Action's.
-}
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ update =
    parseUpdate
        (ChangeName <$  command "change_name"
        <|> GrowCommand maybeChatId <$  command "grow"
        <|> Start maybeChatId <$  command "start"
        <|> ShowInfo <$  command "info"
        <|> InputName maybeChatId <$> text) update
    where
        maybeChatId = chatIdInt update

-- | Action&Model to programm of Action' Model'
handleAction :: Action -> Model -> Eff Action Model
handleAction action model@(Model size name rank time flag) = case action of
    NoAction -> pure model -- nothing to do

    Start chatIdForAction   -> model <# do
                -- to start
        reply (toReplyMessage startMessageText)
            { replyMessageReplyMarkup = Just
                $ Telegram.SomeReplyKeyboardMarkup startMessageKeyboard
            }

        case chatIdForAction of
            Nothing -> liftIO $ return ()
            (Just (ChatId chatIdNumber)) -> liftIO $ DB.addHaskeller (fromInteger chatIdNumber) (name) size ( rank) time
        pure NoAction

    ChangeName -> Model size name rank time True <# do -- change the flag & pure InputName 
        replyText enterNewNameText
        pure NoAction
    
    GrowCommand chatIdForAction -> 
        model <# do
            currentTime <- liftIO getCurrentTime
            if checkForGrowth time currentTime cooldown
            then
                pure (Grow chatIdForAction currentTime) 
            else 
                pure (NotifyThatCannotGrow (cooldown - abs(diffUTCTime time currentTime)))

    InputName chatIdForAction newName -> if flag -- change name if flag, else pure NoAction
        then Model size newName rank time flag <# do
            replyText (changeNameMessageText name newName)

            case chatIdForAction of
                Nothing -> liftIO $ return ()
                (Just (ChatId chatIdNumber)) -> liftIO $ DB.updateName (fromInteger chatIdNumber) newName

            pure NoAction
        else model <# pure NoAction

    ShowInfo -> model <# do -- shows all available information about haskeller
        replyText (statusMessageText name (pack (show size)) rank)
        pure NoAction

    NewRankNotification chatIdForAction newRank -> Model size name newRank time flag <# do -- notifies user about new rank and changes new rank
        replyText (newRankMessageText name newRank)
        case chatIdForAction of
                Nothing -> liftIO $ return ()
                (Just (ChatId chatIdNumber)) -> liftIO $ DB.updateRank (fromInteger chatIdNumber) newRank

        pure NoAction
    
    Grow chatIdForAction newTime -> (Model (size + 1) name rank newTime flag) <# do -- increases IQ by 1
        replyText (growMessageText name (pack (show (size + 1)))) -- If new rank is reached, notifies about it and change it

        case chatIdForAction of
                Nothing -> liftIO $ return ()
                (Just (ChatId chatIdNumber)) -> do 
                        liftIO $ DB.updateIQ (fromInteger chatIdNumber) (size + 1)
                        liftIO $ DB.updateTime (fromInteger chatIdNumber) newTime

        case findNewRank (size + 1) of
            Nothing        -> pure NoAction
            (Just newRank) -> pure (NewRankNotification chatIdForAction newRank)

    NotifyThatCannotGrow deltaTime -> model <# do -- ?
        replyText (cannotGrowMessageText name deltaTime)
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
run :: Telegram.Token -> UTCTime -> [DBModels.Haskeller] -> IO ()
run token time haskellers = do
    env <- Telegram.defaultTelegramClientEnv token
    startBot_ (traceBotDefault (sevBot time haskellers)) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
main :: IO ()
main = do
    {-
    example for db
    now <- getCurrentTime
    DB.addHaskeller 1 "name" 0 "rank" now
    DB.printAll
    DB.updateName 1 "newName"
    DB.updateIQ 1 10
    DB.updateRank 1 "newRank"
    DB.printAll
    DB.getByChatId 1 (print . (++"GET") . show)
    -}
    DB.createHTable
    putStrLn "Please enter telegram token:"
    tgToken     <- getLine

<<<<<<< HEAD
    run (Telegram.Token (pack tgToken))
=======
    haskellersFromDB <- DB.getAll

    putStrLn $ mconcat $ map show haskellersFromDB 

    currentTime <- getCurrentTime
    run (Telegram.Token (pack tgToken)) currentTime haskellersFromDB
>>>>>>> b3273864eb2c71af63d7d2e325c6b28427f6c3a9
