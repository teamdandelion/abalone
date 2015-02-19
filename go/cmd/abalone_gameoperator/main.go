package main

import (
	"flag"
	"fmt"
	"log"
	"time"

	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/game"
)

var (
	aiPort1   = flag.String("aiPort1", "3423", "port for first ai")
	aiPort2   = flag.String("aiPort2", "3424", "port for second ai (if present)")
	timelimit = flag.Duration("timelimit", time.Second*2, "per-move time limit")
)

func main() {
	flag.Parse()
	fmt.Println("game operater operating!")
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	whiteAgent := RemotePlayerInstance{
		APIPlayer: api.Player{},
		Port:      *aiPort1,
	}
	blackAgent := RemotePlayerInstance{
		APIPlayer: api.Player{},
		Port:      *aiPort2,
	}
	start := game.Standard
	result := ExecuteGame(&whiteAgent, &blackAgent, start)

	switch result.VictoryReason {
	case api.InvalidResponse:
		fmt.Println("player submitted an invalid response")
	case api.Timeout:
		fmt.Println("player exceeded", *timelimit, "time limit")
	case api.MovesDepleted:
		fmt.Println("moves depleted")
	case api.StonesDepleted:
		fmt.Println("stones depleted")
	}

	var winner RemotePlayerInstance
	switch result.Outcome {
	case game.WhiteWins:
		winner = whiteAgent
	case game.BlackWins:
		winner = blackAgent
	}
	switch result.Outcome {
	case game.WhiteWins, game.BlackWins:
		fmt.Printf("%s (port %s) wins in %d move(s)", result.Outcome.Winner(), winner.Port, len(result.States))
	default:
		fmt.Println("tie game!")
	}

	return nil
}

type PlayerInstance interface {
	Play(s *game.State, limit time.Duration) (*game.State, error)
	Player() api.Player
}

func toMillisecondCount(d time.Duration) int64 {
	return int64(d / 1e6)
}

func ExecuteGame(whiteAgent, blackAgent PlayerInstance, startState game.State) api.GameResult {
	states := []game.State{startState}
	currentGame := &startState
	victory := api.NoVictory
	outcome := game.NullOutcome
	var moves int = 0
	for !currentGame.GameOver() {
		var nextAI PlayerInstance
		fmt.Printf("move %v (%v)\n", moves, currentGame.NextPlayer.String())
		moves++
		if currentGame.NextPlayer == game.White {
			nextAI = whiteAgent
		} else {
			nextAI = blackAgent
		}
		then := time.Now()
		futureGame, err := nextAI.Play(currentGame, *timelimit)
		if err != nil {
			fmt.Println(err)
			victory = api.InvalidResponse
			outcome = currentGame.NextPlayer.Loses()
			return api.GameResult{
				White:         whiteAgent.Player(),
				Black:         blackAgent.Player(),
				Outcome:       outcome,
				VictoryReason: victory,
				States:        states,
			}
		}
		if enforceLimit(currentGame, moves) && time.Now().Sub(then) > *timelimit {
			return api.GameResult{
				VictoryReason: api.Timeout,

				White:   whiteAgent.Player(),
				Black:   blackAgent.Player(),
				Outcome: currentGame.NextPlayer.Loses(),
				States:  states,
			}
		}
		currentGame = futureGame
		states = append(states, *currentGame)
	}

	outcome = currentGame.Outcome()
	loser := outcome.Loser()
	if loser != game.NullPlayer && currentGame.NumPieces(loser) <= currentGame.LossThreshold {
		victory = api.StonesDepleted
	} else {
		victory = api.MovesDepleted
	}
	return api.GameResult{
		White:         whiteAgent.Player(),
		Black:         blackAgent.Player(),
		Outcome:       outcome,
		VictoryReason: victory,
		States:        states,
	}

}

// first mover is given a bye. maybe we want to give a bye to both players on
// their first moves?
func enforceLimit(_ *game.State, moves int) bool {
	if moves == 1 {
		return false
	}
	return true
}
