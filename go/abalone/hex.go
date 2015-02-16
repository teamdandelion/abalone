package abalone

type Hex struct {
	q, r int
}

func (p *Hex) adjacent(d Direction) Hex {
	q := p.q
	r := p.r
	switch d {
	case TopRight:
		q++
		r--
	case MidRight:
		q++
	case BotRight:
		r++
	case BotLeft:
		q--
		r++
	case MidLeft:
		q--
	case TopLeft:
		r--
	}
	return Hex{q: q, r: r}
}

func abs(x int) int {
	if x >= 0 {
		return x
	} else {
		return -x
	}
}

func (x1 *Hex) dist2Origin() int {
	// twice the distance to the origin (2x so it's always an int)
	return abs(x1.q) + abs(x1.r) + abs(x1.q+x1.r)
}

func hexesEq(h1, h2 []Hex) bool {
	if len(h1) != len(h2) {
		return false
	}
	for i, _ := range h1 {
		if h1[i] != h2[i] {
			return false
		}
	}
	return true
}
