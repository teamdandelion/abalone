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
	aiHost1   = flag.String("aiHost1", "localhost:3423", "port for first ai")
	aiHost2   = flag.String("aiHost2", "localhost:3424", "port for second ai (if present)")
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
	whiteAgent := operator.RemotePlayerInstance{
		APIPlayer: api.Player{},
		Host:      *aiHost1,
	}
	blackAgent := operator.RemotePlayerInstance{
		APIPlayer: api.Player{},
		Host:      *aiHost2,
	}
	result := operator.ExecuteGame(&whiteAgent, &blackAgent, operator.Config{
		Start: game.Standard,
		Limit: *timelimit,
	})

	switch result.VictoryReason {
	case api.TimelimitExceeded:
		fmt.Println(result.VictoryReason, fmt.Sprintln("(limit: %s)", *timelimit))
	default:
		fmt.Println(result.VictoryReason)
	}

	var winner operator.RemotePlayerInstance
	switch result.Outcome {
	case game.WhiteWins:
		winner = whiteAgent
	case game.BlackWins:
		winner = blackAgent
	}
	switch result.Outcome {
	case game.WhiteWins, game.BlackWins:
		fmt.Printf("%s (port %s) wins in %d move(s)", result.Outcome.Winner(), winner.Host, len(result.States))
	default:
		fmt.Println("tie game!")
	}

	return nil
}
