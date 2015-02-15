package abalone

type Direction int

const (
	NullDirection Direction = iota
	TopRight
	MidRight
	BotRight
	BotLeft
	MidLeft
	TopLeft
)

var Directions = []Direction{TopRight, MidRight, BotRight, BotLeft, MidLeft, TopLeft}

func (d *Direction) opposite() Direction {
	switch *d {
	case TopRight:
		return BotLeft
	case MidRight:
		return MidLeft
	case BotRight:
		return TopLeft
	case BotLeft:
		return TopRight
	case MidLeft:
		return MidRight
	case TopLeft:
		return BotRight
	}
	return NullDirection
}

func (d1 *Direction) colinear(d2 Direction) bool {
	return *d1 == d2 || d1.opposite() == d2
}
