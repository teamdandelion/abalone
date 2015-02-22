package api

import "github.com/danmane/abalone/go/game"

// TODO enforce 'non-nullable' on relevant fields

// Author represents a human player
type Author struct {
	Id   int64
	Name string

	Players []Player
}

// Player is an AI agent running on an HTTP server
type Player struct {
	Name    string
	Version int64
	Host    string

	AuthorId int64
	Author Author
	Games  []Game
}

type Game struct {
	Winner  Player
	States  []*game.State
	Outcome Victory

	First  Player
	Second Player
}

type Match struct {
	Players []Player
	Games   []Game
}
type GameResult struct {
	White         Player
	Black         Player
	Outcome       game.Outcome
	VictoryReason Victory
	States        []game.State
}
