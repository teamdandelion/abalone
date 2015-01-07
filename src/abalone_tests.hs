import Abalone
import qualified Data.Set as Set
import qualified Data.List as List

test :: String -> Bool -> IO()
test _ True = return ()
test msg False = putStrLn (msg ++ "- Failed!")

testGameFromBoard :: Board -> Game 
testGameFromBoard b = Game b White 1000 3 

testBoard :: [Position] -> [Position] -> Int -> Board
testBoard whites blacks rad = Board (Set.fromList whites) (Set.fromList blacks) rad

testGame :: [Position] -> [Position] -> Int -> Game 
testGame w b r = testGameFromBoard $ testBoard w b r

testEq :: (Eq a, Show a) => String -> a -> a -> IO()
testEq msg expected actual 
	| expected /= actual = putStrLn $ List.intercalate " " [msg, "expected:", show expected, "got:", show actual]
	| otherwise = return ()

trivialGame = testGame [(0, 0)] [] 5

threeStoneGame = testGame [(-1, 0), (0, 0)] [(1, 0)] 1

threeStoneGameAfterPush = Game (testBoard [(0, 0), (1, 0)] [] 1) Black 999 3

fourStoneGame = testGame [(x, 0) | x <- [-2, -1, 1]] [(0,0)] 5

t1 = testEq "Correct number of moves in trivial circumstance" 6 f where 
	f = (length . futures) trivialGame

t2 = testEq "Correct number of segments in trivial situation" 1 s where
	s = (length . segments) trivialGame

t3 = testEq "Correct number of segments in three stone game" 3 s where
	s = (length . segments) threeStoneGame

t4 = testEq "Correct # moves in 3 stone game" 15 $ (length . futures) threeStoneGame

t5 = testEq "Correct # moves in 4 stone game (piece protection by own pieces respected)" 19 $ (length . futures) fourStoneGame

t6 = testEq "Correct # moves in even game (piece protection by enemy pieces respected)" 14 m where
	m = (length . futures) $ testGame [(-2, 0), (-1, 0)] [(0, 0), (1, 0)] 5 

t7 = test "Stones can be pushed off the board" (threeStoneGameAfterPush `elem` futures threeStoneGame)

threeLinearStoneBoard = testBoard [(0,1), (0,2), (0,3)] [] 5 
t8 = testEq "correct # moves subject to maxMarblesPerMove (1)" 14 m where 
	g = Game threeLinearStoneBoard White 1000 1
	m = (length . futures) g
t9 = testEq "correct # moves subject to maxMarblesPerMove (3)" 30 m where 
	g = Game threeLinearStoneBoard White 1000 3
	m = (length . futures) g 

main :: IO()
main = do
	t1
	t2
	t3
	t4
	t5
	t6
	t7
	t8 
	t9
