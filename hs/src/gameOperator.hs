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

import qualified Player as P 
import Player(Player)
import Abalone(Game, Outcome)
import qualified Abalone as A 

type Agent = Game -> IO (Maybe Game)

main :: IO ()
main = do
	let white = remotePlayer 8001
	let black = remotePlayer 8002 
	outcome <- playStandardGame white black 
	print outcome 

addr :: Int -> String 
addr p = "http://localhost:" ++ show p ++ "/game"

playGame :: Agent -> Game -> IO Outcome
playGame a g0 = do 
	maybeG1 <- a g0
	maybe winnerDueToInvalidResponse continue maybeG1  where 
		winnerDueToInvalidResponse = return $ if A.nextPlayer g0 == P.White then A.BlackWins else A.WhiteWins
		continue :: Game -> IO Outcome 
		continue g = maybe (playGame a g) return (A.winner g)


remotePlayer :: Int -> Agent 
remotePlayer port g = do 
		response <- post (addr port) (Aeson.toJSON g)
		let newGame = Aeson.decode $ response ^. responseBody
		return $ mfilter (A.isValid g) newGame 

combineAgents :: Agent -> Agent -> Agent 
combineAgents white black g = if A.nextPlayer g == P.White then white g else black g 

playStandardGame :: Agent -> Agent -> IO Outcome 
playStandardGame white black = playGame (combineAgents white black) A.start

