package game

import "testing"

var b1 = Board{
	WhitePositions: makeHexSet(Hex{0, 0}, Hex{0, 1}),
	BlackPositions: makeHexSet(Hex{0, -1}),
	EdgeLength:     2,
}

var b1_prime = Board{
	WhitePositions: makeHexSet(Hex{0, 1}, Hex{0, 0}),
	BlackPositions: makeHexSet(Hex{0, -1}),
	EdgeLength:     2,
}

func TestBoardEq(t *testing.T) {
	if !b1.eq(b1_prime) {
		t.Error("equivalent boards were not equal")
	}
}

type ownerTest struct {
	h Hex
	e Player
}

var ownerTests = []ownerTest{
	{h: Hex{0, 0}, e: White},
	{h: Hex{0, -1}, e: Black},
	{h: Hex{1, 1}, e: NullPlayer},
}

func TestOwner(t *testing.T) {
	for _, pair := range ownerTests {
		actual := b1.owner(pair.h)
		if actual != pair.e {
			t.Error("for", pair.h,
				"expected", pair.e,
				"got", actual)
		}
	}
}

type onboardTest struct {
	h Hex
	e bool
}
