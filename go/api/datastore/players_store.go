package datastore

import (
	api "github.com/danmane/abalone/go/api"
	"github.com/jinzhu/gorm"
)

type playersStore struct {
	db *gorm.DB
}

func (s *playersStore) Create(userID int64, p api.Player) (*api.Player, error) {
	p.AuthorId = userID
	if err := s.db.Create(&p).Error; err != nil {
		return nil, err
	}
	return &p, nil
}

func (s *playersStore) List() ([]api.Player, error) {
	var players []api.Player
	if err := s.db.Find(&players).Error; err != nil {
		return nil, err
	}
	return players, nil
}

func (s *playersStore) Delete(id int64) error {
	return s.db.Delete(api.Player{ID: id}).Error
}

var _ api.PlayersService = &playersStore{}
