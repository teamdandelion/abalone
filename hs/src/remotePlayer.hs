{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
import qualified Data.Aeson as Aeson
import GHC.Generics
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Web.Scotty
import Network.Wai.Middleware.RequestLogger

import Player
import Abalone

main :: IO ()
main = scotty 8002 $ do
	middleware logStdoutDev
	runAbalone

runAbalone :: ScottyM ()
runAbalone = do
	get "/" showLandingPage
	post "/game" respondToGame

respondToGame :: ActionM ()
respondToGame = do
    g <- Aeson.fromJSON <$> jsonData
    case g of
         Aeson.Success game -> json $ stupidAI game
         --TODO(brian): return a 400 error
         Aeson.Error _ -> return ()

showLandingPage :: ActionM ()
showLandingPage = do
	setHeader "Content-Type" "text/html"
	html "hello world"

stupidAI :: Game -> Game
stupidAI = head . futures
