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

type Agent = Game -> Maybe Game 

main :: IO ()
main = do
	let white = remotePlayer 8001
	let black = remotePlayer 8002 
	liftM2 playStandardGame white black >>= print 

addr :: Int -> String 
addr p = "http://localhost:" ++ show p ++ "/game"

remotePlayer :: Int -> IO Agent 
remotePlayer port g = do 
	response <- post (addr port) (Aeson.toJSON g)
	let newGame = Aeson.decode $ response ^. responseBody
	return mfilter (A.isValid g) newGame 

processGameStream :: [(Maybe Game, Outcome)] -> Outcome
processGameStream ((g,o):xs) = maybe o (\x -> fromMaybe (processGameStream xs) (A.winner x)) g

combineAgents :: Agent -> Agent -> Agent 
combineAgents white black g = if A.nextPlayer g == P.White then white g else black g 

gameStream :: Game -> Agent -> [(Maybe Game, Outcome)]
gameStream start combinedAgent = iterate f (Just start, A.BlackWins) where 
	f (game, outcome) = (game >>= combinedAgent, alternate outcome)
	alternate A.WhiteWins = A.BlackWins
	alternate A.BlackWins = A.WhiteWins 

playGame :: Game -> Agent -> Agent -> Outcome
playGame g white black = processGameStream . (gameStream g) $ combineAgents white black 

playStandardGame :: Agent -> Agent -> Outcome 
playStandardGame = playGame A.start 

