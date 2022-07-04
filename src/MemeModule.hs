{-# LANGUAGE OverloadedStrings #-}

module MemeModule where


import System.IO

import Telegram.Bot.API.Types
import Telegram.Bot.API.Methods

import qualified Data.Text as T


filesWithMemes :: T.Text
filesWithMemes = "memes"

getMemePath :: Integer -> Maybe String
getMemePath iq
	| iq > 100 = Nothing
	| otherwise = Just $ T.unpack $ mconcat [filesWithMemes, "/", (T.pack $ show numberOfMeme), ".jpg"]
		where 
			numberOfMeme = iq `div` 10


getInputFile :: Integer -> Maybe InputFile
getInputFile iq = do
	res <- getMemePath iq

	Just $ InputFile res "multipart/form-data"

getPhotoFile :: Integer -> Maybe PhotoFile
getPhotoFile iq = do
	res <- getInputFile iq

	Just $ MakePhotoFile res