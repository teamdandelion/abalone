package game

import "encoding/json"

type HexSet map[Hex]struct{}

func (s HexSet) MarshalJSON() ([]byte, error) {
	return json.Marshal(s.toSlice())
}

func (s *HexSet) UnmarshalJSON(data []byte) error {
	*s = make(HexSet)
	var in []Hex
	if err := json.Unmarshal(data, &in); err != nil {
		return err
	}
	for _, h := range in {
		(*s)[h] = struct{}{}
	}
	return nil
}

func (set HexSet) toSlice() []Hex {
	res := make([]Hex, len(set))
	i := 0
	for position, _ := range set {
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
