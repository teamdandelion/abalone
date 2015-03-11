package api

import (
	"fmt"

	"github.com/danmane/abalone/go/game"
)

type Game struct {
	ID int64 `gorm:"column:id"`

	MatchId int64 `gorm:"column:match_id"`
	WhiteId int64 `gorm:"column:white_player_id"`
	BlackId int64 `gorm:"column:black_player_id"`

	Reason string `gorm:"column:reason"`
	Status string `gorm:"column:status"`

	CommonDBFields
}

func (g *Game) Outcome() (game.Outcome, error) {

	// TODO(btc): Game outcome and game status are somewhat redundant. Look for
	// ways to simplify and merge these enums.

	switch GameStatus(g.Status) {
	// normal cases
	case GameWhiteWins:
		return game.WhiteWins, nil
	case GameBlackWins:
		return game.BlackWins, nil
	case GameDraw:
		return game.Tie, nil

	case GameScheduled:
		// TODO(btc): it is my understanding that NullOutcome is meant to be
		// used for games in progress. Where the DB is concerned, GameScheduled
		// is presently used for games that haven't terminated, so NullOutcome
		// seems like an okay choice for now.
		// We'll probably want to condense/share a single representation. This
		// smells like trouble down the road.
		return game.NullOutcome, nil

	default:
		return 0, fmt.Errorf("unrecognized outcome: %s", g.Status)
	}
}

type GameWithDetails struct {
	Game
	WhitePlayer Player
	BlackPlayer Player
}

func (c GameWithDetails) TableName() string {
	return "games"
}

type GameStatus string

const (
	GameScheduled GameStatus = "scheduled"
	GameDraw      GameStatus = "draw"
	GameBlackWins GameStatus = "black_wins"
	GameWhiteWins GameStatus = "white_wins"
)

func (s GameStatus) String() string {
	return string(s)
}

type GameResult struct {
	White         Player
	Black         Player
	Outcome       game.Outcome
	VictoryReason Victory
	States        []game.State
}
