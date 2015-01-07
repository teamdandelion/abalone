import Player
import Abalone
import Data.List

type Depth = Int
type Score = Int
type Heuristic = Game -> Score
type AI = Game -> Game

play :: AI -> Maybe Player
play ais = (winner . head) $ dropWhile (not . gameOver) $ iterate ais start 

combineAIs :: AI -> AI -> AI
combineAIs white black = \g -> if nextPlayer g == White then white g else black g

stupid :: AI -- very stupid ai, always takes first move offered
stupid = head . futures

search :: Game -> Heuristic -> Depth -> Game
search g0 h d = snd $ searchR h d g0 where
	self = nextPlayer g0
	cmp = \(s1, g1) (s2, g2) -> s1 `compare` s2
	searchR :: Heuristic -> Depth -> Game -> (Score, Game)
	searchR h d g
		| d == 0 = (h g, g)
		| otherwise = choose cmp $ map (searchR h (d-1)) (futures g) where
			choose = if nextPlayer g == self then maximumBy else minimumBy

countSegments :: Player -> Game -> Int
countSegments p g = length $ segments_ (marblesPerMove g) (board g) p

h1 :: Player -> Heuristic
h1 p = (\g -> numPieces g p * 100 + countSegments p g)

aiFromSearch :: Player -> AI 
aiFromSearch p = (\g -> search g (h1 p) 2)

round1AI = combineAIs stupid (aiFromSearch Black)
round2AI = combineAIs (aiFromSearch White) stupid

winner1 = play round1AI
winner2 = play round2AI 

main :: IO()
main = do 
	print winner1
	print winner2
