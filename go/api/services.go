package api

import (
	"io"

	"github.com/jinzhu/gorm"
)

type Services struct {
	Rankings RankingsService
	Games    GamesService
	Matches  MatchesService
	Players  PlayersService
	Users    UsersService

	DB *gorm.DB
}

type RankingsService interface {
	List() ([]*Ranking, error)
}

type Ranking struct {
	Rank   int     `json:"rank"`
	Player string  `json:"player"`
	Author string  `json:"author"`
	Rating float64 `json:"rating"`
	Wins   int     `json:"wins"`
	Losses int     `json:"losses"`
}

type GamesService interface {
	List() ([]Game, error)
	ListDetailed() ([]*GameWithDetails, error)
}

type PlayersService interface {
	Upload(userID int64, p Player, executable io.Reader) (*Player, error)
	Create(userID int64, p Player) (*Player, error)
	List() ([]Player, error)
	Delete(id int64) error
}

type UsersService interface {
	Create(User) (*User, error)
	List() ([]User, error)
	Delete(id int64) error
}

type MatchesService interface {
	// Run creates a match and schedules it for execution.
	Run(playerID1, playerID2 int64) (*Match, error)

	// Create creates a match.
	// TODO Create(playerID1, playerID2 int64) (*Match, error)
}
