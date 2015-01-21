{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Aeson as Aeson
import GHC.Generics
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Network.Wreq 
import Data.Maybe
import Data.Text

import qualified Player
import Player(Player)
import Abalone(Game, Outcome)
import qualified Abalone

main :: IO ()
main = undefined 

addr :: Int -> Text 
addr p = "http://localhost:" ++ show p 

remotePlayer :: Int -> Game -> IO Game 
remotePlayer port = undefined

playGame :: (Game -> IO Game) -> (Game -> IO Game) -> IO Outcome
playGame white black = recurGame Abalone.start where 
	recurGame :: Game -> IO Outcome 
	recurGame g 
		| Abalone.gameOver g = return . fromJust . Abalone.winner $ g 
		| otherwise = getPlayerF (Abalone.nextPlayer g) g >>= recurGame 

	getPlayerF :: Player -> Game -> IO Game
	getPlayerF Player.White = white 
	getPlayerF Player.Black = black 


