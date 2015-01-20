{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
import Data.Aeson
import GHC.Generics
import Control.Applicative
import Control.Monad


import Web.Scotty
import Network.Wai.Middleware.RequestLogger

import Player
import Abalone 

data GameResponse = GameResponse {   id :: Int
								 , game :: Game
								 } deriving (Eq, Show, Read, Generic)
instance FromJSON GameResponse
instance ToJSON   GameResponse

main :: IO ()
main = scotty 8001 $ do 
	middleware logStdoutDev 
	runAbalone 

runAbalone :: ScottyM () 
runAbalone = do 
	get "/" showLandingPage
	post "/gamestate" updateGame


updateGame :: ActionM () 
updateGame = do 
	response <- fromJSON <$> jsonData 
	case response of 
		Error r -> putStr r 

showLandingPage :: ActionM ()
showLandingPage = do 
	setHeader "Content-Type" "text/html"
	html "hello world"