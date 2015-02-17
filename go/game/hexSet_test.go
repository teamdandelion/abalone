package game

func makeHexSet(hexes ...Hex) HexSet {
	return slice2HexSet(hexes)
}
