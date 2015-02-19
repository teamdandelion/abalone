package main

import (
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net/http"
	"time"

	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/game"
)

var (
	playAgainstHuman = flag.Bool("playAgainstHuman", false, "play against human on frontend rather than AI vs AI")
	humanPort        = flag.String("humanPort", "1337", "port for javascript frontend")
	aiPort1          = flag.String("aiPort1", "3423", "port for first ai")
	aiPort2          = flag.String("aiPort2", "3424", "port for second ai (if present)")
	timelimit        = flag.Duration("timelimit", time.Second*2, "per-move time limit")
)

func main() {
	flag.Parse()
	fmt.Println("game operater operating!")
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	whiteAI := api.Player{}
	blackAI := api.Player{}
	whiteAgent := PlayerInstance{Player: whiteAI, Port: *aiPort1}
	blackAgent := PlayerInstance{Player: blackAI, Port: *aiPort2}
	start := game.Standard
	playAIGame(whiteAgent, blackAgent, start)
	// fmt.Println(result)
	return nil
}

type PlayerInstance struct {
	Player api.Player
	Port   string
}

func toMillisecondCount(d time.Duration) int64 {
	return int64(d / 1e6)
}

func gameFromAI(port string, state *game.State) (*game.State, error) {
	mr := api.MoveRequest{
		State:      *state,
		LimitMilli: toMillisecondCount(api.DefaultMoveLimit),
	}
	var buf bytes.Buffer
	if err := json.NewEncoder(&buf).Encode(&mr); err != nil {
		return nil, err
	}
	resp, err := http.Post("http://localhost:"+port+api.MovePath, "application/json", &buf)
	if err != nil {
		return nil, err
	}
	responseGame := &game.State{}
	if err := json.NewDecoder(resp.Body).Decode(responseGame); err != nil {
		return nil, err
	}
	resp.Body.Close()
	if !state.ValidFuture(responseGame) {
		return nil, fmt.Errorf("game parsed correctly, but isn't a valid future")
	}
	return responseGame, nil
}

func playAIGame(whiteAgent, blackAgent PlayerInstance, startState game.State) api.GameResult {
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
		futureGame, err := gameFromAI(nextAI.Port, currentGame)
		if err != nil {
			fmt.Println("Game is terminating due to an invalid response!")
			fmt.Println(err)
			victory = api.InvalidResponse
			outcome = currentGame.NextPlayer.Loses()
			return api.GameResult{
				White:         whiteAgent.Player,
				Black:         blackAgent.Player,
				Outcome:       outcome,
				VictoryReason: victory,
				States:        states,
			}
		}
		currentGame = futureGame
		states = append(states, *currentGame)
	}

	outcome = currentGame.Outcome()
	if outcome == game.WhiteWins {
		fmt.Printf("white wins (on port %v)\n", *aiPort1)
	} else if outcome == game.BlackWins {
		fmt.Printf("black wins (on port %v)\n", *aiPort2)
	} else {
		fmt.Println("tie game!")
	}
	loser := outcome.Loser()
	if loser != game.NullPlayer && currentGame.NumPieces(loser) <= currentGame.LossThreshold {
		fmt.Println("stones depeleted")
		victory = api.StonesDepleted
	} else {
		fmt.Println("moves depeleted")
		victory = api.MovesDepleted
	}
	return api.GameResult{
		White:         whiteAgent.Player,
		Black:         blackAgent.Player,
		Outcome:       outcome,
		VictoryReason: victory,
		States:        states,
	}

}
