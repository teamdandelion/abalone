package api

import "github.com/danmane/abalone/go/game"

type Game struct {
	Winner  Player
	States  []*game.State
	Outcome Victory

	First  Player
	Second Player
}

type GameResult struct {
	White         Player
	Black         Player
	Outcome       game.Outcome
	VictoryReason Victory
	States        []game.State
}
