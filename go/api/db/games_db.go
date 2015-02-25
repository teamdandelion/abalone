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
	var results []api.GameWithDetails
	if err := s.db.Raw(`SELECT
							games.id, 
				  			games.match_id,
				  			white.name as white_name,
				  			white.version as white_version,
				  			black.name as black_name,
				  			black.version as black_version,
				  			games.status,
				  			games.reason
				  		FROM games
				  		LEFT JOIN players white ON
				  			games.white_player_id = white.id
				  		LEFT JOIN players black ON
				  		  	games.black_player_id = black.id`).Scan(&results).Error; err !=  nil{
		return nil, err
	}
	return results, nil
}

var _ api.GamesService = new(gamesDB)
