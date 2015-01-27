{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe 
import System.Environment

import Web.Scotty
import Network.Wai.Middleware.RequestLogger

main :: IO () 
main = initiate 8955 

initiate :: Int -> IO ()
initiate port = scotty port $ do 
	middleware logStdoutDev
	runRelay

runRelay :: ScottyM () 
runRelay = do 
	get "/poll" respondPoll
	post "/game" recieveGame 
	post "/elm-response" dispatchGame 

respondPoll :: ActionM ()
-- if we are have a recieved game state, respond with it
-- otherwise, give a 204 

receiceGame :: ActionM () 

dispatchGame :: ActionM () 
-- if we a


{--
The relay acts as an intermediary between the gameOperator and the elm frontend.
The issue is that players are expected to recieve POST requests with a game state and then 
	respond to the request with a new game state. But, our javascript frontend cannot 
	recieve post requests. So, the relay recieves POST requests and the elm frontend will
	poll the relay with GET requests. Once the relay has a new gamestate, it replies to 
	the polling with the new gamestate. It then waits for a POST request from the frontend with
	the frontend player decision, and returns it to the gameOperator as a response.
--}

