package db

import "github.com/jinzhu/gorm"

// TODO enforce 'non-nullable' on relevant fields

// Author represents a human player
type Author struct {
	Id   int64
	Name string

	Players []Player
}

// Player is an AI agent running on an HTTP server
type Player struct {
	Id      int64
	Nick    string // represents a series of agents
	Version int64  // represents a version within a Series
	Address string // host:port of the HTTP server

	AuthorId int64
}

type StartState struct {
	Id   int64
	JSON string // json
}

type Game struct {
	Id int64

	StartState StartState
	Winner     Player
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

// AutoMigrate ensures all tables and columns are up to date (non-destructive)
func AutoMigrate(sql *gorm.DB) *gorm.DB {
	return sql.AutoMigrate(
		&Author{},
		&Player{},
		&StartState{},
		&Game{},
		&Participant{},
		&Record{},
	)
}
