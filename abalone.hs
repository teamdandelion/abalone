data AbaloneGame = AbaloneGame {
	whitePositions :: [Position],
	blackPositions :: [Position],
	nextPlayer 	   :: Player,
	turnCounter    :: Int,
	rules          :: AbaloneRules
} deriving (Show)

data AbaloneRules = AbaloneRules {
	boardRadius :: Int,
	maxMarblesPerMove :: Int,
	movesUntilEnd :: Int
} deriving (Show)

data Player = White | Black deriving (Eq, Show)
type Position = (Int, Int)

gameOver :: AbaloneGame -> Bool
gameOver g = turnCounter g >= rules . movesUntilEnd g 
				|| length whitePositions g == 0 
				|| length blackPositions g == 0

winner :: AbaloneGame -> Maybe Player
winner g 
	| gameOver g = advantage g
	| otherwise  = Nothing
	where
	advantage :: AbaloneGame -> Maybe Player 
	advantage g
		| numPieces White > numPieces Black = Just White
		| numPieces White < numPieces Black = Just Black
		| otherwise							= Nothing


numPieces :: AbaloneGame -> Player -> Int
numPieces g p 
	| p == White = length . whitePositions g
	| p == Black = length . blackPositions g 