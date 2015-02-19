package main

import (
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"net/http"
	"time"

	"github.com/cenkalti/backoff"
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
	result := playAIGame(&whiteAgent, &blackAgent, start)

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

type RemotePlayerInstance struct {
	APIPlayer api.Player
	Port      string
}

func (i *RemotePlayerInstance) Player() api.Player {
	return i.APIPlayer
}

func (i *RemotePlayerInstance) Play(s *game.State, limit time.Duration) (*game.State, error) {
	return gameFromAI(i.Port, s)
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

	var rawResponse bytes.Buffer // will contain response if HTTP request is successful
	f := func() error {
		resp, err := http.Post("http://localhost:"+port+api.MovePath, "application/json", bytes.NewBuffer(buf.Bytes()))
		if err != nil {
			return err
		}
		defer resp.Body.Close()
		io.Copy(&rawResponse, resp.Body)
		return nil
	}

	if err := withRetries(f); err != nil {
		return nil, err
	}

	responseGame := &game.State{}
	if err := json.NewDecoder(&rawResponse).Decode(responseGame); err != nil {
		return nil, err
	}
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

func withRetries(f func() error) error {
	backoffConfig := backoff.NewExponentialBackOff()
	backoffConfig.InitialInterval = time.Second
	backoffConfig.MaxInterval = 10 * time.Second
	backoffConfig.MaxElapsedTime = 60 * time.Second

	notifyFunc := func(err error, dur time.Duration) {
		log.Printf("waiting %v, failed to get move from player: %s", dur, err)
	}
	err := backoff.RetryNotify(f, backoffConfig, notifyFunc)
	return err
}
