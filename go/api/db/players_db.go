package db

import (
	api "github.com/danmane/abalone/go/api"
	"github.com/jinzhu/gorm"
)

type playersDB struct {
	db *gorm.DB
}

func (s *playersDB) Create(userID int64, p api.Player) (*api.Player, error) {
	p.AuthorId = userID
	if err := s.db.Create(&p).Error; err != nil {
		return nil, err
	}
	return &p, nil
}

func (s *playersDB) List() ([]api.Player, error) {
	var players []api.Player
	if err := s.db.Find(&players).Error; err != nil {
		return nil, err
	}
	return players, nil
}

func (s *playersDB) Delete(id int64) error {
	return s.db.Delete(api.Player{ID: id}).Error
}

var _ api.PlayersService = &playersDB{}
