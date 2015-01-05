
module Abalone (
	AbaloneGame, AbaloneRules, Player(White, Black), Position,
	gameOver, winner, numPieces) where

import qualified Data.Set as Set

data AbaloneBoard = AbaloneBoard {
	whitePositions :: Set.Set Position,
	blackPositions :: Set.Set Position,
	boardRadius    :: Int
}

data AbaloneGame = AbaloneGame {
	board 		   :: AbaloneBoard,
	nextPlayer 	   :: Player,
	movesRemaining :: Int,
	marblesPerMove :: Int,
} deriving (Eq, Show, Read)

data Player = White | Black deriving (Eq, Show, Read, Ord, Bounded)
type Position = (Int, Int)

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
isValid g0 g1 
	| marblesPerMove g0 /= marblesPerMove g1 = False
	| nextPlayer g0 == nextPlayer g1 = False
	| movesRemaining g0 /= movesRemaining g1 + 1 = False
	| otherwise = isValid (board g0) (board g1)

isValid :: AbaloneBoard -> AbaloneBoard -> Bool
