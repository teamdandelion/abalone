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
	Id int64
}

type GameResult struct {
	Id            int64
	White         Player
	Black         Player
	Outcome       game.Outcome
	VictoryReason Victory
	States        []game.State
}

type Participant struct {

	// TODO primary key is game_id, order

	Game   Game
	Player Player
	Order  int64 // eg. first to play
}

type Record struct {
	Game Game
	Turn int64
	Move []byte // 6-byte diff
}

type AgentInfo struct {
	Owner  string
	Taunts []string
}
