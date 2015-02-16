package abalone

type Game struct {
	board          Board `json:"board"`
	nextPlayer     Player `json:"nextPlayer"`
	movesRemaining int `json:"movesRemaining"`
	marblesPerMove int `json:"marblesPerMove"`
	lossThreshold  int `json:"lossThreshold"`
}

var StandardGame Game = Game{
	board:          standardBoard,
	nextPlayer:     White,
	movesRemaining: 1000,
	marblesPerMove: 3,
	lossThreshold:  8,
}

func (g *Game) segments() []Segment {
	pieces := g.board.pieces(g.nextPlayer)
	result := make([]Segment, 0, 3*len(pieces))
	for pos, _ := range pieces {
		s := Segment{
			base:        pos,
			length:      1,
			player:      g.nextPlayer,
			orientation: NullDirection,
		}
		result = append(result, s)
		for d := TopRight; d <= BotRight; d++ {
			next := pos.adjacent(d)
			length := 2
			for length <= g.marblesPerMove && pieces.has(next) {
				s = Segment{
					base:        pos,
					orientation: d,
					length:      length,
					player:      g.nextPlayer,
				}
				next = next.adjacent(d)
				length++
				result = append(result, s)
			}
		}
	}
	return result
}

func (g *Game) moves() []Move {
	result := make([]Move, 0)
	for _, s := range g.segments() {
		for _, d := range Directions {
			m := Move{segment: s, direction: d}
			if m.isValid(g) {
				result = append(result, m)
			}
		}
	}
	return result
}

// like all functions in this implementation, this returns a copy
// if given an invalid move, behavior is undefined
func (g *Game) Update(m *Move) Game {
	ownPieces := m.segment.segPieces()
	var enemyPieces []Hex
	if m.inline() {
		enemyPieces = m.inlineMoved(g.board)
	} else {
		enemyPieces = make([]Hex, 0)
	}
	var whiteMoved, blackMoved []Hex
	if g.nextPlayer == White {
		whiteMoved = ownPieces
		blackMoved = enemyPieces
	} else {
		whiteMoved = enemyPieces
		blackMoved = ownPieces
	}

	copyAndMove := func(original HexSet, hexesToMove []Hex) HexSet {
		result := make(HexSet)
		for hex, _ := range original {
			result[hex] = struct{}{}
		}
		for _, hex := range hexesToMove {
			delete(result, hex)
		}
		for _, hex := range hexesToMove {
			adj := hex.adjacent(m.direction)
			if g.board.onBoard(adj) {
				result[adj] = struct{}{}
			}
		}
		return result
	}

	newWhite := copyAndMove(g.board.whitePositions, whiteMoved)
	newBlack := copyAndMove(g.board.blackPositions, blackMoved)
	newBoard := Board{
		whitePositions: newWhite,
		blackPositions: newBlack,
		edgeLength:     g.board.edgeLength,
	}
	newGame := Game{
		board:          newBoard,
		nextPlayer:     g.nextPlayer.Next(),
		movesRemaining: g.movesRemaining - 1,
		marblesPerMove: g.marblesPerMove,
		lossThreshold:  g.lossThreshold,
	}

	return newGame
}

func (g *Game) Futures() []Game {
	moves := g.moves()
	result := make([]Game, len(moves))
	for i := 0; i < len(moves); i++ {
		result[i] = g.Update(&moves[i])
	}
	return result
}

func (g1 *Game) eq(g2 Game) bool {
	return g1.board.eq(g2.board) &&
		g1.nextPlayer == g2.nextPlayer &&
		g1.movesRemaining == g2.movesRemaining &&
		g1.lossThreshold == g2.lossThreshold &&
		g1.marblesPerMove == g2.marblesPerMove

}

func (g1 *Game) Valid(g2 Game) bool {
	found := false
	for _, future := range g1.Futures() {
		future.eq(g2)
	}
	return found
}

func (g *Game) GameOver() bool {
	return g.Winner() != NullOutcome
}

func (g *Game) Winner() Outcome {
	w := len(g.board.whitePositions)
	b := len(g.board.blackPositions)
	if g.movesRemaining <= 0 || w <= g.lossThreshold || b <= g.lossThreshold {
		if w < b {
			return BlackWins
		} else if b < w {
			return WhiteWins
		} else {
			return Tie
		}
	} else {
		return NullOutcome
	}
}
