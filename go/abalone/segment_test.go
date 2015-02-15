package abalone

import "testing"

type testpair struct {
	segment Segment
	values  []Hex
}

var tests = []testpair{
	{
		Segment{
			base:        Hex{0, 0},
			length:      1,
			player:      NullPlayer,
			orientation: NullDirection,
		},
		[]Hex{{0, 0}}},
	{
		Segment{
			base:        Hex{0, 0},
			length:      3,
			player:      NullPlayer,
			orientation: MidRight,
		},
		[]Hex{{0, 0}, {1, 0}, {2, 0}},
	},
}

func TestSegPieces(t *testing.T) {
	for _, pair := range tests {
		v := pair.segment.segPieces()
		if !hexesEq(pair.values, v) {
			t.Error("For", pair.segment, "expected", pair.values, "got", v)
		}
	}
}
