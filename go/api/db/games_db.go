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
	if err := s.db.Find(&games).Error; err != nil {
		return nil, err
	}
	var players []api.Player
	if err := s.db.Find(&players).Error; err != nil {
		return nil, err
	}
	for i := range games {
		for j := range players {
			if games[i].WhiteID == players[j].ID {
				games[i].White = players[j]
			}
			if games[i].BlackID == players[j].ID {
				games[i].Black = players[j]
			}
		}
	}
	return games, nil
}

var _ api.GamesService = new(gamesDB)
