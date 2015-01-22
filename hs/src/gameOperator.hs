{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Aeson as Aeson
import GHC.Generics
import Control.Applicative
import Control.Monad
import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text
import Data.Aeson.Lens (key)

import Network.Wreq 

import qualified Player
import Player(Player)
import Abalone(Game, Outcome)
import qualified Abalone

main :: IO ()
main = do
	let white = remotePlayer 8001
	let black = remotePlayer 8002 
	playGame white black >>= print 

addr :: Int -> String 
addr p = "http://localhost:" ++ show p ++ "/game"

remotePlayer :: Int -> Game -> IO Game 
remotePlayer port g = do 
	response <- post (addr port) (Aeson.toJSON g)
	let newGame = fromJust $ Aeson.decode $ response ^. responseBody
	return newGame

playGame :: (Game -> IO Game) -> (Game -> IO Game) -> IO Outcome
playGame white black = recurGame Abalone.start where 
	recurGame :: Game -> IO Outcome 
	recurGame g 
		| Abalone.gameOver g = return . fromJust . Abalone.winner $ g 
		| otherwise = getPlayerF (Abalone.nextPlayer g) g >>= recurGame 

	getPlayerF :: Player -> Game -> IO Game
	getPlayerF Player.White = white 
	getPlayerF Player.Black = black 


