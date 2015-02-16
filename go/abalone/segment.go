package abalone

type Segment struct {
	base        Hex
	length      int
	player      Player
	orientation Direction
}

func (s *Segment) segPieces() []Hex {
	result := make([]Hex, s.length)
	p := s.base
	for i := 0; i < s.length; i++ {
		result[i] = p
		p = p.adjacent(s.orientation)
	}
	return result
}
