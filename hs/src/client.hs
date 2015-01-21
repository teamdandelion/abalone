{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
import qualified Data.Aeson as Aeson
import GHC.Generics
import Control.Applicative
import Control.Monad


import Web.Scotty
import Network.Wai.Middleware.RequestLogger

import Player
import qualified Abalone as Abalone 

main :: IO ()
main = scotty 8001 $ do 
	middleware logStdoutDev 
	runAbalone 

runAbalone :: ScottyM () 
runAbalone = do 
	get "/" showLandingPage
	post "/game" respondToGame 

respondToGame :: ActionM ()
respondToGame = do 
	-- game :: ActionM Game 
	game <- jsonData 
	-- nextState :: ActionM Game 
	nextState <- fmap stupidAI game 
	json nextState 


showLandingPage :: ActionM ()
showLandingPage = do 
	setHeader "Content-Type" "text/html"
	html "hello world"

stupidAI :: Abalone.Game -> Abalone.Game 
stupidAI = head . Abalone.futures 