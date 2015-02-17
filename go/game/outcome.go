package game

type Outcome int

const (
	NullOutcome Outcome = iota
	WhiteWins
	BlackWins
	Tie
)
