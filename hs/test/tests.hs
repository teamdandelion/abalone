{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit

import Abalone
import Player

import qualified Data.Set as Set
import Data.List 
import Data.Aeson
import Data.Maybe (fromJust)

testGameFromBoard :: Board -> Game 
testGameFromBoard b = Game b White 1000 3 

testBoard :: [Position] -> [Position] -> Int -> Board
testBoard whites blacks rad = Board (Set.fromList whites) (Set.fromList blacks) rad

testGame :: [Position] -> [Position] -> Int -> Game 
testGame w b r = testGameFromBoard $ testBoard w b r

testEq :: (Eq a, Show a) => String -> a -> a -> IO()
testEq msg expected actual 
	| expected /= actual = putStrLn $ intercalate " " [msg, "expected:", show expected, "got:", show actual]
	| otherwise = return ()

trivialGame = testGame [(0, 0)] [] 5

threeStoneGame = testGame [(-1, 0), (0, 0)] [(1, 0)] 1

threeStoneGameAfterPush = Game (testBoard [(0, 0), (1, 0)] [] 1) Black 999 3

fourStoneGame = testGame [(x, 0) | x <- [-2, -1, 1]] [(0,0)] 5

numMoves :: Game -> Int
numMoves = length . futures

numSegments :: Game -> Int
numSegments = length . segments


threeLinearStoneBoard = testBoard [(0,1), (0,2), (0,3)] [] 5 

main :: IO ()
main = defaultMain $ 
	testGroup "Abalone Unit Tests" [
		testGroup "numMoves" [
			testCase "Correct number of moves in 1 stone game" $ 
				numMoves trivialGame @?= 6,

			testCase "Correct # moves in 3 stone game" $
				numMoves threeStoneGame @?= 15,

			testCase "Correct # moves in 4 stone game (piece protection by own pieces respected)" $
				numMoves fourStoneGame @?= 19,

			testCase "Correct # moves in even game (piece protection by enemy pieces respected)" $ 
				numMoves (testGame [(-2, 0), (-1, 0)] [(0, 0), (1, 0)] 5) @?= 14,

			testCase "correct # moves subject to maxMarblesPerMove (1)" $
				numMoves (Game threeLinearStoneBoard White 1000 1) @?= 14,

			testCase "correct # moves subject to maxMarblesPerMove (3)" $
				numMoves (Game threeLinearStoneBoard White 1000 3) @?= 30

		], testGroup "numSegments" [
			testCase "Correct number of segments in 1 stone game" $ 
				numSegments trivialGame @?= 1,

			testCase "Correct number of segments in three stone game" $ 
				numSegments threeStoneGame @?= 3
		],
		-- todo - test that inline push works in both directions

		testCase "Stones can be pushed off the board" $ 
			(threeStoneGameAfterPush `elem` futures threeStoneGame) @?= True,

		testGroup "serialization Tests" [
			testCase "standard board serializes appropriately" $ 
				encode start @?= "{\"marblesPerMove\":3,\"movesRemaining\":200,\"nextPlayer\":\"White\",\"board\":{\"whitePositions\":[[-4,3],[-4,4],[-3,3],[-3,4],[-2,2],[-2,3],[-2,4],[-1,2],[-1,3],[-1,4],[0,2],[0,3],[0,4],[1,3]],\"boardRadius\":5,\"blackPositions\":[[-1,-3],[0,-4],[0,-3],[0,-2],[1,-4],[1,-3],[1,-2],[2,-4],[2,-3],[2,-2],[3,-4],[3,-3],[4,-4],[4,-3]]}}",

			testCase "deserialization works as expected" $ 
				(fromJust . decode . encode) start @?= start
		]
	]




