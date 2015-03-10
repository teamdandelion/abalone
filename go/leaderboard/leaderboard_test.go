package leaderboard

import (
	"fmt"
	"github.com/danmane/abalone/go/game"
	"math/rand"
	"testing"
)

var rankingTests = []struct {
	numPlayers int
	games      []Result
	rankings   []int
	name       string
}{
	{2, []Result{}, []int{1, 1}, "2p - no games"},
	{2, []Result{Result{whiteID: 0, blackID: 1, outcome: game.Tie}}, []int{1, 1}, "2p - tie"},
	{2, []Result{Result{whiteID: 0, blackID: 1, outcome: game.WhiteWins}}, []int{1, 2}, "2p - one wins"},
	{3, []Result{Result{whiteID: 0, blackID: 1, outcome: game.BlackWins}}, []int{3, 1, 2}, "3p - one wins"},
	{3, []Result{
		Result{whiteID: 0, blackID: 1, outcome: game.BlackWins},
		Result{whiteID: 1, blackID: 2, outcome: game.WhiteWins},
		Result{whiteID: 0, blackID: 2, outcome: game.BlackWins},
	}, []int{3, 1, 2}, "3p - triangle of games"},
}

func Test_Rankings(t *testing.T) {
	for _, test := range rankingTests {
		rankings := RateGames(test.numPlayers, test.games)
		for _, r := range rankings {
			if test.rankings[r.playerID] != r.rank {
				fmt.Printf("test: %v\n, \trankings: %v\n", test.name, rankings)
				t.Errorf("%v: expected rank %v for id %v but got %v", test.name, test.rankings[r.playerID], r.playerID, r.rank)
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
	nPlayers := 100
	gamesPerPlayer := 100
	results := make([]Result, 0)
	for i := 0; i < nPlayers; i++ {
		for j := 0; j < gamesPerPlayer; j++ {
			opponent := rand.Intn(nPlayers)
			result := Result{whiteID: i, blackID: opponent, outcome: randOutcome()}
			results = append(results, result)
		}
	}
	for n := 0; n <= b.N; n++ {
		RateGames(nPlayers, results)
	}
}
