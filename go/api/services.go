package api

import (
	"github.com/danmane/abalone/go/game"
	"github.com/jinzhu/gorm"
)

type Services struct {
	Players PlayersService
	Games   GamesService
	States  GameStateService

	DB *gorm.DB
}

type PlayersService interface {
	List() ([]Player, error) // TODO(btc) fix signature
	Create() error
}

type GamesService interface {
	Create(initial game.State) (*game.State, error)
	Find(gameID int) (*game.State, error)
	List(offset, limit int) []game.State
}

type GameStateService interface {
	List(gameID, offset, limit int) ([]game.Board, error)
	Create(gameID int, state game.Board, n int) error
}
