package abalone

type Board struct {
	whitePositions HexSet
	blackPositions HexSet
	edgeLength     int
}

func (b *Board) pieces(p Player) HexSet {
	if p == White {
		return b.whitePositions
	} else {
		return b.blackPositions
	}
}

func (b *Board) free(x Hex) bool {
	return b.owner(x) == NullPlayer
}

func (b1 *Board) eq(b2 Board) bool {
	return b1.whitePositions.eq(b2.whitePositions) &&
		b1.blackPositions.eq(b2.blackPositions) &&
		b1.edgeLength == b2.edgeLength
}

func (b *Board) owner(x Hex) Player {
	if b.whitePositions.has(x) {
		return White
	} else if b.blackPositions.has(x) {
		return Black
	} else {
		return NullPlayer
	}
}

func (b *Board) onBoard(x Hex) bool {
	return x.dist2Origin() < b.edgeLength*2
}

var standardBoard Board = Board{
	edgeLength: 5,
	whitePositions: slice2HexSet(
		[]Hex{
			{-4, 3}, {-4, 4}, {-3, 3}, {-3, 4}, {-2, 2},
			{-2, 3}, {-2, 4}, {-1, 2}, {-1, 3}, {-1, 4},
			{0, 2}, {0, 3}, {0, 4}, {1, 3},
		},
	),
	blackPositions: slice2HexSet(
		[]Hex{
			{-1, -3}, {0, -4}, {0, -3}, {0, -2}, {1, -4},
			{1, -3}, {1, -2}, {2, -4}, {2, -3}, {2, -2},
			{3, -4}, {3, -3}, {4, -4}, {4, -3},
		},
	),
}
