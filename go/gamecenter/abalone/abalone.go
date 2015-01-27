package abalone

func init() {
	DefaultGameState = GameState{
		LossThreshold:  8,
		MarblesPerMove: 3,
		MovesRemaining: 200,
		NextPlayer:     White,
		Board: Board{
			WhitePositions: []Position{}, // TODO
			BlackPositions: []Position{}, // TODO
		},
	}
}

var DefaultGameState GameState

type GameState struct {
	Board          Board  `json:"board"`
	NextPlayer     Player `json:"nextPlayer"`
	MovesRemaining int    `json:"movesRemaining"`
	MarblesPerMove int    `json:"marblesPerMove"`
	LossThreshold  int    `json:"lossThreshold"`
}

type Board struct {
	WhitePositions []Position `json:"whitePositions"`
	BlackPositions []Position `json:"blackPositions"`
}

type Player string

const (
	White = "White"
	Black = "Black"
)

type Position [2]int
