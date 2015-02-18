package main

import (
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net/http"

	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/game"
)

var mem = flag.Bool("mem", false, "run with an in-memory backed")
var host = flag.String("host", ":8080", "set the http host addr:port")

var (
	playAgainstHuman = flag.Bool("playAgainstHuman", false, "play against human on frontend rather than AI vs AI")
	humanPort        = flag.String("humanPort", "1337", "port for javascript frontend")
	aiPort1          = flag.String("aiPort1", "3423", "port for first ai")
	aiPort2          = flag.String("aiPort2", "3424", "port for second ai (if present)")
)

func main() {
	flag.Parse()
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
	result := playAIGame(whiteAgent, blackAgent, start)
	fmt.Println(result)
	return nil
}

type PlayerInstance struct {
	Player api.Player
	Port   string
}

func gameFromAI(port string, state *game.State) (error, *game.State) { // TODO reverse args
	var buf bytes.Buffer
	if err := json.NewEncoder(&buf).Encode(state); err != nil {
		return err, nil
	}
	resp, err := http.Post("http://localhost:"+port+"/move", "application/json", &buf)
	if err != nil {
		return err, nil
	}
	responseGame := &game.State{}
	if err := json.NewDecoder(resp.Body).Decode(responseGame); err != nil {
		return err, nil
	}
	resp.Body.Close()
	if !state.ValidFuture(responseGame) {
		return fmt.Errorf("game parsed correctly, but isn't a valid future"), nil
	}
	return nil, responseGame
}

func playAIGame(whiteAgent, blackAgent PlayerInstance, startState game.State) api.GameResult {
	states := []game.State{startState}
	currentGame := &startState
	victory := api.NoVictory
	outcome := game.NullOutcome
	for !currentGame.GameOver() {
		var nextAI PlayerInstance
		if currentGame.NextPlayer == game.White {
			nextAI = whiteAgent
		} else {
			nextAI = blackAgent
		}
		err, futureGame := gameFromAI(nextAI.Port, currentGame)
		if err != nil {
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

	outcome = currentGame.Winner()
	if currentGame.MovesRemaining == 0 {
		// TODO win on last move = stones depleted
		victory = api.MovesDepleted
	} else {
		victory = api.StonesDepleted
	}
	return api.GameResult{
		White:         whiteAgent.Player,
		Black:         blackAgent.Player,
		Outcome:       outcome,
		VictoryReason: victory,
		States:        states,
	}

}
