package abalone

type HexSet map[Hex]struct{}

func (hm *HexSet) hexSet2Slice() []Hex {
	res := make([]Hex, len(*hm))
	i := 0
	for position, _ := range *hm {
		res[i] = position
		i++
	}
	return res
}

func (m1 HexSet) eq(m2 HexSet) bool {
	if len(m1) != len(m2) {
		return false
	}
	for x, _ := range m1 {
		if !m2.has(x) {
			return false
		}
	}
	return true
}

func (s HexSet) has(h Hex) bool {
	_, found := s[h]
	return found
}

func slice2HexSet(hexes []Hex) HexSet {
	res := make(HexSet)
	for _, h := range hexes {
		res[h] = struct{}{}
	}
	return res
}
