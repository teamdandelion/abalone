package rankings

import (
	"fmt"
	"math/rand"
	"testing"

	"github.com/bradfitz/iter"
	"github.com/danmane/abalone/go/game"
)

var rankingTests = []struct {
	players  []int64
	games    []Result
	rankings []int
	name     string
}{
	{nil, []Result{}, []int{1, 1}, "2p - no games"},
	{[]int64{0, 1}, []Result{Result{WhiteID: 0, BlackID: 1, Outcome: game.Tie}}, []int{1, 1}, "2p - tie"},
	{[]int64{0, 1}, []Result{Result{WhiteID: 0, BlackID: 1, Outcome: game.WhiteWins}}, []int{1, 2}, "2p - one wins"},
	{[]int64{0, 1}, []Result{Result{WhiteID: 0, BlackID: 1, Outcome: game.BlackWins}}, []int{2, 1, 3}, "3p - one wins"},
	{[]int64{0, 1, 2}, []Result{
		Result{WhiteID: 0, BlackID: 1, Outcome: game.BlackWins},
		Result{WhiteID: 1, BlackID: 2, Outcome: game.WhiteWins},
		Result{WhiteID: 0, BlackID: 2, Outcome: game.BlackWins},
	}, []int{3, 1, 2}, "3p - triangle of games"},
}

func Test_Rankings(t *testing.T) {
	for _, test := range rankingTests {
		rankings, err := RateGames(test.players, test.games)
		if err != nil {
			t.Fatal(err)
		}
		for _, r := range rankings {
			if test.rankings[r.PlayerID] != r.Rank {
				fmt.Printf("test: %v\n, \trankings: %v\n", test.name, rankings)
				t.Errorf("%v: expected rank %v for id %v but got %v", test.name, test.rankings[r.PlayerID], r.PlayerID, r.Rank)
			}
		}
	}
}

func randOutcome() (o game.Outcome) {
	r := rand.Intn(3)
	switch r {
	case 0:
		o = game.WhiteWins
	case 1:
		o = game.BlackWins
	default:
		o = game.Tie
	}
	return
}

func Benchmark_Rankings(b *testing.B) {
	var players []int64

	for i := range iter.N(100) {
		players = append(players, int64(i))
	}
	gamesPerPlayer := 100
	results := make([]Result, 0)
	for _, white := range players {
		for j := 0; j < gamesPerPlayer; j++ {
			opponent := players[rand.Intn(len(players))]
			result := Result{WhiteID: white, BlackID: opponent, Outcome: randOutcome()}
			results = append(results, result)
		}
	}
	b.ResetTimer()
	for n := 0; n <= b.N; n++ {
		if _, err := RateGames(players, results); err != nil {
			b.Fatal(err)
		}
	}
}
