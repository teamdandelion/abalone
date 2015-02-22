package api

type Victory int

const (
	NoVictory Victory = iota
	MovesDepleted
	StonesDepleted
	InvalidResponse
	TimelimitExceeded
)

func (v Victory) String() string {
	switch v {
	case MovesDepleted:
		return "moves depleted"
	case StonesDepleted:
		return "stones depleted"
	case InvalidResponse:
		return "player returned an invalid response"
	case TimelimitExceeded:
		return "player exceeded time limit"
	default: // NoVictory
		return "no victory"
	}
}
