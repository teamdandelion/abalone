package datastore

import (
	"fmt"
	"log"

	api "github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/game"
	"github.com/danmane/abalone/go/operator"
	"github.com/jinzhu/gorm"
)

type matchesStore struct {
	db *gorm.DB
}

func (s *matchesStore) Run(playerID1, playerID2 int64) (*api.Match, error) {
	matchrequest := api.Match{
		PID1: playerID1,
		PID2: playerID2,
	}
	if err := s.db.Create(&matchrequest).Error; err != nil {
		return nil, err
	}
	var players []api.Player
	if err := s.db.Where([]int64{playerID1, playerID2}).Find(&players).Error; err != nil {
		return nil, err
	}
	if len(players) != 2 {
		return nil, fmt.Errorf("error retrieving players. %d player(s) found", len(players))
	}
	go func() {
		a := players[0]
		b := players[1]
		whiteAgent := operator.RemotePlayerInstance{
			APIPlayer: a,
			Host:      a.Host,
		}
		blackAgent := operator.RemotePlayerInstance{
			APIPlayer: b,
			Host:      b.Host,
		}
		result := operator.ExecuteGame(&whiteAgent, &blackAgent, operator.Config{
			Start: game.Standard,
			Limit: api.DefaultMoveLimit,
		})
		log.Println(result)

		// TODO handle game creation
	}()
	return &matchrequest, nil
}

func (s *matchesStore) List() ([]api.Player, error) {
	var players []api.Player
	if err := s.db.Find(&players).Error; err != nil {
		return nil, err
	}
	return players, nil
}

func (s *matchesStore) Delete(id int64) error {
	return s.db.Delete(api.Player{ID: id}).Error
}

var _ api.MatchesService = &matchesStore{}
