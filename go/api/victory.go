package api

type Victory int

const (
	NoVictory Victory = iota
	MovesDepleted
	StonesDepleted
	InvalidResponse
	Timeout
)
