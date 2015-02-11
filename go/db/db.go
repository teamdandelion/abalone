package db

import "github.com/jinzhu/gorm"

// TODO enforce 'non-nullable' on relevant fields

type Author struct {
	Id   int64
	Name string

	Players []Player
}

type Player struct {
	Id        int64
	Nick      string
	Version   int64
	Container string

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
	Move []byte
}

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
