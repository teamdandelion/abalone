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
	marblesPerMove :: Int,
} deriving (Eq, Show, Read)

data Player = White | Black deriving (Eq, Show, Read, Ord, Bounded)
type Position = (Int, Int)

data Move = Move {
	segment   :: Segment,
	direction :: Direction,
} deriving (Eq, Show, Read)

data Segment = Segment {
	head        :: Position,
	orientation :: Direction,
	length      :: Int,
} deriving (Eq, Show, Read)

data Direction = TopRight | MidRight | BotRight 
			   | BotLeft  | MidLeft  | TopLeft deriving (Eq, Show, Read, Ord, Bounded)

getPieces :: AbaloneBoard -> Player -> Set.Set Position
getPieces b p = if p == White then whitePositions else blackPositions $ b

segments :: AbaloneGame -> [Segment]
segments g = segments (marblesPerMove g) (nextPlayer g) (board g)

segments :: Int -> AbaloneBoard -> Player -> [Segment]
segments len b p = flatten $ map f positionDirectionPairs where
	pieces = getPieces b p 
	positionDirectionPairs = zip (toList pieces) [TopRight, MidRight, BotRight]
	f :: (Position, Direction) -> [Segment]
	f (p, d) = map (Segment p d) [1..numSegments] where
		tails = iterate (adjacent (opposite d)) p
		validTails = takeWhile (\p -> p `member` pieces) tails
		numSegments = min (length validTails) len

adjacent :: Direction -> Position -> Position

opposite :: Direction -> Direction
opposite d = case d of 
	TopRight -> BotLeft
	MidRight -> MidLeft
	BotRight -> TopLeft
	BotLeft  -> TopRight
	MidLeft  -> MidRight
	TopLeft  -> BotRight


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