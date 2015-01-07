module Abalone (
	Game(Game), nextPlayer, marblesPerMove, board, 
	Board(Board), Position,
	gameOver, winner, numPieces, futures, isValid,
	Direction, Segment, segments, segments_, adjacent, start
	) where

import qualified Data.Set as Set
import Player

data Game = Game {
	board 		   :: Board,
	nextPlayer 	   :: Player,
	movesRemaining :: Int,
	marblesPerMove :: Int
} deriving (Eq, Show, Read)

data Board = Board {
	whitePositions :: Set.Set Position,
	blackPositions :: Set.Set Position,
	boardRadius    :: Int
} deriving (Eq, Show, Read)

getPieces :: Board -> Player -> Set.Set Position
getPieces b p = (if p == White then whitePositions else blackPositions) b

start :: Game 
start = Game standardBoard White 200 3

standardBoard :: Board
standardBoard = Board (Set.fromList whitePos) (Set.fromList blackPos) 5 where
	whitePos = [(-x, 4) | x <- [0..4]] ++ [(-x, 3) | x <- [-1..4]] ++ [(-2, 2), (-1, 2), (0, 2)]
	blackPos = map (\(q, r) -> (-q, -r)) whitePos

-- Position / Grid Functions --
type Position = (Int, Int)
dist2 :: Position -> Position -> Int -- distance * 2 (to avoid fractional types)
dist2 (q1,r1) (q2,r2) = abs(q1 - q2) + abs(r1 - r2) + abs(q1 + r1 - q2 - r2)

data Direction = TopRight | MidRight | BotRight 
			   | BotLeft  | MidLeft  | TopLeft 
			   deriving (Eq, Show, Read, Ord, Bounded, Enum)

adjacent :: Direction -> Position -> Position
adjacent d (q, r) = case d of 
	TopRight -> (q+1, r-1)
	MidRight -> (q+1, r  )
	BotRight -> (q  , r+1)
	BotLeft  -> (q-1, r+1)
	MidLeft  -> (q-1, r  )
	TopLeft  -> (q  , r-1)

opposite :: Direction -> Direction
opposite d = case d of 
	TopRight -> BotLeft
	MidRight -> MidLeft
	BotRight -> TopLeft
	BotLeft  -> TopRight
	MidLeft  -> MidRight
	TopLeft  -> BotRight

colinear :: Direction -> Direction -> Bool
colinear d1 d2 = d1 `elem` [d2, opposite d2]

-- Moves are internal-only representation of a move - external API is just game states
data Move = Move {
	segment   :: Segment,
	direction :: Direction
} deriving (Eq, Show, Read)

-- A segment is a linear group of marbles that could move. 
data Segment = Segment {
	basePos     :: Position, -- The start position of the segment
	orientation :: Direction, -- The direction the segment grows in (irrelevant if len is 1)
	segLength   :: Int -- The length of the segment
} deriving (Eq, Show, Read)

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
	| p == White = Set.size . whitePositions . board $ g
	| p == Black = Set.size . blackPositions . board $ g 

-- this function will recieve new game states from client and verify validity
isValid :: Game -> Game -> Bool
-- very inefficient impl but that should be fine since occurs once per turn
isValid g0 g1 = g1 `elem` (futures g0)

onBoard :: Board -> Position -> Bool -- is a piece on the board still?
onBoard board pos = dist2 pos (0, 0) <= (boardRadius board) * 2

update :: Game -> Move -> Game 
update (Game b p remaining perMove) (Move (Segment pos orient len) dir) = newGame where
	newGame = Game newBoard (next p) (remaining - 1) perMove
	ownPieces = take len $ iterate (adjacent orient) pos
	enemyPieces = if colinear orient dir then recur start (len-1) else [] where 
		start = adjacent dir $ if orient == dir then last ownPieces else pos
		enemy = \x -> x `Set.member` getPieces b (next p)
		recur x force 
			| not $ enemy x = []
			| force == 0    = []
			| otherwise     = x : recur (adjacent dir x) (force - 1)

	updated = filter (onBoard b) . map (adjacent dir)

	whiteMoved = if p == White then ownPieces else enemyPieces
	blackMoved = if p == Black then ownPieces else enemyPieces

	removeAndAdd :: Ord a => Set.Set a -> [a] -> [a] -> Set.Set a
	removeAndAdd s toRemove toAdd = Set.union (Set.fromList toAdd) removed where
		removed = Set.difference s (Set.fromList toRemove)

	white = removeAndAdd (whitePositions b) whiteMoved (updated whiteMoved)
	black = removeAndAdd (blackPositions b) blackMoved (updated blackMoved)

	newBoard = Board white black (boardRadius b)

futures :: Game -> [Game] -- find all valid future states of this board (1 move)
futures g = map (update g) (possibleMoves g) where 
	{-- algorithm:
	- find all segments (distinct groupings that can move)
	- take cartesian product with all directions
	- see if that direction is a valid move for given segment
	- if orientation is aligned with direction, attempt a "forward" move - might push off
		opponent so more complex computation
	- if orientation is not aligned with direction, attempt a "broadside" - just check
		that all destination spaces are free
	--}
	possibleMoves :: Game -> [Move]
	possibleMoves g = filter isMove allMoves where
		allMoves = crossApply (map Move (segments g)) [TopRight .. TopLeft]
		b = board g
		own   p = Set.member p $ getPieces b $ nextPlayer g
		enemy p = Set.member p $ getPieces b $ next $ nextPlayer g
		free  p = (not . own) p && (not . enemy) p
		isMove (Move (Segment pos orient len) dir) = valid where 
			valid = if colinear dir orient then forward else broadside 

			broadside = all free destination where
				destination = take len $ iterate (adjacent orient) $ adjacent dir pos

			forward = iterF (adjacent dir front) (len - 1) where
				front = if (orient==dir) then otherEnd else pos
				otherEnd = iterate (adjacent dir) pos !! (len - 1)
				iterF pos force 
					| free pos   = True
					| own  pos   = False
					| force == 0 = False
					| otherwise  = iterF (adjacent dir pos) (force - 1)

-- surely there's a more elegant way to implement this...
cross :: [a] -> [b] -> [(a, b)]
cross as bs = concat $ map (\a -> map (\b -> (a, b)) bs) as 

-- can i rewrite this pointsfree? would have hoped `map (\(a, b) -> a b) $ cross` would work
crossApply :: [a -> b] -> [a] -> [b]
crossApply as bs = map (\(a, b) -> a b) (cross as bs)

-- get every segement (distinct linear grouping) for current player in game
-- handle singletons seperately because otherwise they could be triple-counted
segments :: Game -> [Segment]
segments g = segments_ (marblesPerMove g) (board g) (nextPlayer g)
segments_ :: Int -> Board -> Player -> [Segment]
segments_ len b p = singletons ++ lengthTwoOrMore where
	pieces = getPieces b p 
	pieceList = Set.toList pieces
	singletons = map (\x -> Segment x TopRight 1) pieceList
	positionDirectionPairs = cross (Set.toList pieces) [TopRight, MidRight, BotRight]
	lengthTwoOrMore = concat $ map f positionDirectionPairs
	f :: (Position, Direction) -> [Segment]
	f (p, d) = map (Segment p d) [2..numSegments] where
		tails = iterate (adjacent d) p
		validTails = takeWhile (\p -> p `Set.member` pieces) tails
		numSegments = min (length validTails) len