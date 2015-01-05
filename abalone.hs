module Abalone (
	AbaloneGame, AbaloneBoard, Player(White, Black), Position,
	gameOver, winner, numPieces) where

import qualified Data.Set as Set

data AbaloneBoard = AbaloneBoard {
	whitePositions :: Set.Set Position,
	blackPositions :: Set.Set Position,
	boardRadius    :: Int
} deriving (Eq, Show, Read)

data AbaloneGame = AbaloneGame {
	board 		   :: AbaloneBoard,
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
	head        :: Position,
	orientation :: Direction,
	segLength   :: Int
} deriving (Eq, Show, Read)

data Direction = TopRight | MidRight | BotRight 
			   | BotLeft  | MidLeft  | TopLeft deriving (Eq, Show, Read, Ord, Bounded)

getPieces :: AbaloneBoard -> Player -> Set.Set Position
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


gameOver :: AbaloneGame -> Bool
gameOver g = movesRemaining g <= 0 || any (\p -> numPieces g p == 0) [White, Black]

winner :: AbaloneGame -> Maybe Player
winner g = if gameOver g then advantage else Nothing where
	w = numPieces g White
	b = numPieces g Black
	advantage 
		| w > b     = Just White
		| w < b     = Just Black
		| otherwise	= Nothing

numPieces :: AbaloneGame -> Player -> Int
numPieces g p 
	| p == White = Set.size . whitePositions $ g
	| p == Black = Set.size . blackPositions $ g 

isValid :: AbaloneGame -> AbaloneGame -> Bool
-- very inefficient impl but that should be fine
isValid g0 g1 = g1 `elem` map (update g0) (possibleMoves g0)

update :: AbaloneGame -> Move -> AbaloneGame

possibleMoves :: AbaloneGame -> [Move]
possibleMoves g = filter isMove allMoves where
	allMoves = concat $ map (map Move (segments g)) [TopRight .. TopLeft]
	b = board g
	own   p = Set.member p $ getPieces b $ nextPlayer g
	enemy p = Set.member p $ getPieces b $ next $ nextPlayer g
	free  p = (not . own) p && (not . enemy) p
	isMove (Move (Segment pos orient len) dir) = if aligned then forward else broadside where 
		aligned = dir `elem` [orient, opposite orient] where 

		broadside = all free destination where
			destination = take len $ iterate (adjacent orient) $ adjacent dir pos

		forward = iterF (adjacent dir front) (len - 1) where
			front = if (orient == dir) then iterate (adjacent dir) pos !! len - 1 else pos
			iterF pos force 
				| free pos   = True
				| own  pos   = False
				| force == 0 = False
				| otherwise  = iterF (adjacent dir pos) (force - 1)


cross :: [a] -> [b] -> [(a, b)]
cross as bs = concat $ map (\a -> map (\b -> (a, b)) bs) as 

segments :: AbaloneGame -> [Segment]
segments g = segments_ (marblesPerMove g) (nextPlayer g) (board g) where
	segments_ :: Int -> AbaloneBoard -> Player -> [Segment]
	segments_ len b p = concat $ map f positionDirectionPairs where
		pieces = getPieces b p 
		positionDirectionPairs = cross (Set.toList pieces) [TopRight, MidRight, BotRight]
		f :: (Position, Direction) -> [Segment]
		f (p, d) = map (Segment p d) [1..numSegments] where
			tails = iterate (adjacent d) p
			validTails = takeWhile (\p -> p `Set.member` pieces) tails
			numSegments = min (segLength validTails) len