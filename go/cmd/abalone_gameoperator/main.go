package main

import (
	"flag"
	"fmt"
	"log"
	"time"

	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/game"
	"github.com/danmane/abalone/go/operator"
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
	result := operator.ExecuteGame(&whiteAgent, &blackAgent, operator.Config{
		Start: game.Standard,
		Limit: *timelimit,
	})

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

func toMillisecondCount(d time.Duration) int64 {
	return int64(d / 1e6)
}
