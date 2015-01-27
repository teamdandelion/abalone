package abalone

type GameState struct {
	Board          Board  `json:"board"`
	NextPlayer     Player `json:"nextPlayer"`
	MovesRemaining int    `json:"movesRemaining"`
	MarblesPerMove int    `json:"marblesPerMove"`
	LossThreshold  int    `json:"lossThreshold"`
}

func NewGameState() *GameState {
	gs := &GameState{
		LossThreshold:  8,
		MarblesPerMove: 3,
		MovesRemaining: 200,
		NextPlayer:     WhitePlayer,
		Board: Board{
			WhitePositions: []Position{}, // TODO
			BlackPositions: []Position{}, // TODO
		},
	}

	return gs
}

type Board struct {
	WhitePositions []Position `json:"whitePositions"`
	BlackPositions []Position `json:"blackPositions"`
}

type Player string

const (
	WhitePlayer = "WhitePlayer"
	BlackPlayer = "BlackPlayer"
)

type Position [2]int
