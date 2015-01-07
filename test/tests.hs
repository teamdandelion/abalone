import Test.Tasty
import Test.Tasty.HUnit

import Abalone
import Player

import qualified Data.Set as Set
import Data.List 

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

		testCase "Stones can be pushed off the board" $ 
			(threeStoneGameAfterPush `elem` futures threeStoneGame) @?= True
	]