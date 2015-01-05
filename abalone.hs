module Abalone (
	Game, Board, Player(White, Black), Position,
	gameOver, winner, numPieces) where

import qualified Data.Set as Set

data Board = Board {
	whitePositions :: Set.Set Position,
	blackPositions :: Set.Set Position,
	boardRadius    :: Int
} deriving (Eq, Show, Read)

data Game = Game {
	board 		   :: Board,
	nextPlayer 	   :: Player,
	movesRemaining :: Int,
	marblesPerMove :: Int
} deriving (Eq, Show, Read)

data Player = White | Black deriving (Eq, Show, Read, Ord, Bounded)
type Position = (Int, Int)

data Move = Move {
	segment   :: Segment,
	direction :: Direction
} deriving (Eq, Show, Read)

data Segment = Segment {
	basePos     :: Position,
	orientation :: Direction,
	segLength   :: Int
} deriving (Eq, Show, Read)

data Direction = TopRight | MidRight | BotRight 
			   | BotLeft  | MidLeft  | TopLeft deriving (Eq, Show, Read, Ord, Bounded)

getPieces :: Board -> Player -> Set.Set Position
getPieces b p = if p == White then whitePositions else blackPositions $ b

adjacent :: Direction -> Position -> Position
adjacent d p = p -- TODO: fix this

opposite :: Direction -> Direction
opposite d = case d of 
	TopRight -> BotLeft
	MidRight -> MidLeft
	BotRight -> TopLeft
	BotLeft  -> TopRight
	MidLeft  -> MidRight
	TopLeft  -> BotRight

next :: Player -> Player
next p = case p of
	White -> Black
	Black -> White


gameOver :: Game -> Bool
gameOver g = movesRemaining g <= 0 || any (\p -> numPieces g p == 0) [White, Black]

winner :: Game -> Maybe Player
winner g = if gameOver g then advantage else Nothing where
	w = numPieces g White
	b = numPieces g Black
	advantage 
		| w > b     = Just White
		| w < b     = Just Black
		| otherwise	= Nothing

numPieces :: Game -> Player -> Int
numPieces g p 
	| p == White = Set.size . whitePositions $ g
	| p == Black = Set.size . blackPositions $ g 

isValid :: Game -> Game -> Bool
-- very inefficient impl but that should be fine
isValid g0 g1 = g1 `elem` map (update g0) (possibleMoves g0)

colinear :: Direction -> Direction -> Bool
colinear d1 d2 = d1 `elem` [d2, opposite d2]

update :: Game -> Move -> Game
update (Game b p remaining perMove) (Move (Segment pos orient len) dir) = newGame where
	newGame = Game newBoard (next p) (remaining - 1) perMove
	ownPieces = take len $ iterate (adjacent orient) pos
	enemyPieces = if colinear orient dir then recur start (len-1) else [] where 
		start = adjacent dir $ if orient == dir then last ownPieces else pos
		enemy = \x -> x `Set.member` getPieces b (next p)
		recur x force 
			| not . enemy x = []
			| force == 0    = []
			| otherwise     = x : recur (adjacent dir x) (force - 1)

	radius = boardRadius b
	onBoard = 
	updated = filter onBoard . map (adjacent dir)

	whiteMoved = if p == White then ownPieces else enemyPieces
	blackMoved = if p == Black then ownPieces else enemyPieces

	removeAndAdd :: Set.Set a -> [a] -> [a] -> Set.Set a
	removeAndAdd s toRemove toAdd = Set.union (Set.fromList toAdd) removed where
		removed = Set.difference s (Set.fromList toRemove)

	white = removeAndAdd (whitePositions b) whiteMoved (updated whiteMoved)
	black = removeAndAdd (blackPositions b) blackMoved (updated blackMoved)

	newBoard = Board white black (boardRadius b)


possibleMoves :: Game -> [Move]
possibleMoves g = filter isMove allMoves where
	allMoves = concat $ map (map Move (segments g)) [TopRight .. TopLeft]
	b = board g
	own   p = Set.member p $ getPieces b $ nextPlayer g
	enemy p = Set.member p $ getPieces b $ next $ nextPlayer g
	free  p = (not . own) p && (not . enemy) p
	isMove m = if colinear dir orient then forward else broadside where 
		dir     = direction m
		orient  = orientation . segment m
		len     = segLength   . segment m
		pos     = basePos        . segment m
		aligned = dir `elem` [orient, opposite orient] 

		broadside = all free destination where
			destination = take len $ iterate (adjacent orient) $ adjacent dir pos

		forward = iterF (adjacent dir front) (len - 1) where
			front = if (orient == dir) then iterate (adjacent dir) pos !! len-1 else pos
			iterF pos force 
				| free pos   = True
				| own  pos   = False
				| force == 0 = False
				| otherwise  = iterF (adjacent dir pos) (force - 1)


cross :: [a] -> [b] -> [(a, b)]
cross as bs = concat $ map (\a -> map (\b -> (a, b)) bs) as 

segments :: Game -> [Segment]
segments g = segments_ (marblesPerMove g) (nextPlayer g) (board g) where
	segments_ :: Int -> Board -> Player -> [Segment]
	segments_ len b p = concat $ map f positionDirectionPairs where
		pieces = getPieces b p 
		positionDirectionPairs = cross (Set.toList pieces) [TopRight, MidRight, BotRight]
		f :: (Position, Direction) -> [Segment]
		f (p, d) = map (Segment p d) [1..numSegments] where
			tails = iterate (adjacent d) p
			validTails = takeWhile (\p -> p `Set.member` pieces) tails
			numSegments = min (segLength validTails) len