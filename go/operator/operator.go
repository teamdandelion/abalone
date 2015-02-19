package operator

import (
	"fmt"
	"time"

	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/game"
)

type PlayerInstance interface {
	Play(s *game.State, limit time.Duration) (*game.State, error)
	Player() api.Player
}

type Config struct {
	Start game.State
	Limit time.Duration
}

func ExecuteGame(whiteAgent, blackAgent PlayerInstance, config Config) api.GameResult {
	states := []game.State{config.Start}
	currentGame := &config.Start
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
		futureGame, err := nextAI.Play(currentGame, config.Limit)
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
		if enforceLimit(currentGame, moves) && time.Now().Sub(then) > config.Limit {
			return api.GameResult{
				VictoryReason: api.TimelimitExceeded,

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
