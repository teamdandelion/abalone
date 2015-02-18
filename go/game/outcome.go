package game

type Outcome int

const (
	NullOutcome Outcome = iota
	WhiteWins
	BlackWins
	Tie
)

func (o Outcome) Winner() Player {
	if o == WhiteWins {
		return White
	} else if o == BlackWins {
		return Black
	} else {
		return NullPlayer
	}
}

func (o Outcome) Loser() Player {
	return o.Winner().Next()
}
