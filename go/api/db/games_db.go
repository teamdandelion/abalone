package db

import (
	api "github.com/danmane/abalone/go/api"
	"github.com/jinzhu/gorm"
)

type gamesDB struct {
	db *gorm.DB
}

func (s *gamesDB) List() ([]api.Game, error) {
	var games []api.Game
	if err := s.db.Find(&games).Error; err != nil {
		return nil, err
	}
	return games, nil
}

func (s *gamesDB) ListDetailled() ([]api.GameWithDetails, error) {
	var games []api.GameWithDetails
	var whites []api.Player
	var blacks []api.Player
	if err := s.db.Model(&games).Related(&whites, "WhiteID").Error; err !=  nil{
		return nil, err
	}
	if err := s.db.Model(&games).Related(&blacks, "BlackID").Error; err !=  nil{
		return nil, err
	}
	return games, nil
}

var _ api.GamesService = new(gamesDB)
