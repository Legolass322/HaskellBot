{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Telegram.Bot.API          as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser
import System.Environment


type Size = Int
type Name = Text
-- | Bot conversation state model.
data Model = Model Size Name
    deriving (Show)

-- | Actions bot can perform.
data Action
  = NoAction    -- ^ Perform no action.
  | Grow
  | Name Text
  deriving (Show)


-- | Bot application.
bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model 0
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

-- | How to process incoming 'Telegram.Update's
-- and turn them into 'Action's.
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate $ 
    Name <$> text <$> command "name" <|>
    Grow    

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction action model@(Model size name) = case action of
    NoAction -> pure model
    Name newName -> (Model size newName) <# do
        replyText "Name changed"
        pure NoAction
    Grow -> (Model (size + 1) name) <# do
        replyText "Grow + 1"
        pare NoAction


-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault bot) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
main :: IO ()
main = do
    setEnv "TELEGRAM_BOT_TOKEN" "2026002626:AAE_oOm3jXXJ2lsZJ_0TvsSPpF13W_lqw6I"
    getEnvToken "TELEGRAM_BOT_TOKEN" >>= run