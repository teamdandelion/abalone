package abalone

type Player int

const (
	NullPlayer Player = iota
	White
	Black
)

func (p Player) Next() Player {
	switch p {
	case White:
		return Black
	case Black:
		return White
	default:
		return NullPlayer
	}
}
