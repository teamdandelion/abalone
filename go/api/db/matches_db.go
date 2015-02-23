package db

import (
	"fmt"
	"log"
	"encoding/json"
	"bytes"

	api "github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/game"
	"github.com/danmane/abalone/go/operator"
	"github.com/jinzhu/gorm"
)

type matchesDB struct {
	db *gorm.DB
}

func (s *matchesDB) Run(playerID1, playerID2 int64) (*api.Match, error) {
	matchrequest := api.Match{
		PID1: playerID1,
		PID2: playerID2,
	}
	if err := s.db.Create(&matchrequest).Error; err != nil {
		return nil, err
	}
	if err := ExecuteMatch(s.db, matchrequest); err != nil {
		return nil, err
	}
	return &matchrequest, nil
}

func (s *matchesDB) List() ([]api.Player, error) {
	var players []api.Player
	if err := s.db.Find(&players).Error; err != nil {
		return nil, err
	}
	return players, nil
}

func (s *matchesDB) Delete(id int64) error {
	return s.db.Delete(api.Player{ID: id}).Error
}

var _ api.MatchesService = &matchesDB{}

type Policy struct {
	Configs []MatchConfig
}

type MatchConfig struct {
	Start game.State
}

func ExecuteMatch(db *gorm.DB, m api.Match) error {

	const (
		policyNumGames = 2
	)

	var games []api.Game
	if err := db.Where(api.Match{ID: m.ID}).Find(&games).Error; err != nil {
		return err
	}

	constraints := struct {
		PID1AsWhite bool
		PID1AsBlack bool

		PID2AsWhite bool
		PID2AsBlack bool
	}{}

	for _, g := range games {
		if g.WhiteId == m.PID1 {
			constraints.PID1AsWhite = true
		}
		if g.BlackId == m.PID1 {
			constraints.PID1AsBlack = true
		}
		if g.BlackId == m.PID2 {
			constraints.PID2AsBlack = true
		}
		if g.WhiteId == m.PID2 {
			constraints.PID2AsWhite = true
		}
	}

	// player one is white doesn't equal player two as black
	// or vice-versa
	if (constraints.PID1AsWhite != constraints.PID2AsBlack) || (constraints.PID1AsBlack != constraints.PID2AsWhite) {
		return fmt.Errorf("couldn't run games. database inconsistency detected. match: %+v, constraints: %+v", m, constraints)
	}

	// https://www.youtube.com/watch?v=zyu2jAD6sdo

	if !constraints.PID1AsWhite {
		// run game where player 1 is white
		g := api.Game{
			Status:  api.GameScheduled.String(),
			WhiteId: m.PID1,
			BlackId: m.PID2,
			MatchId: m.ID,
		}
		if err := db.Create(&g).Error; err != nil {
			return err
		}
		go func() {
			if err := run(db, g); err != nil {
				log.Println(err)
			}
		}()
	}

	if !constraints.PID2AsWhite {
		// run game where player 2 is white
		g := api.Game{
			Status:  api.GameScheduled.String(),
			WhiteId: m.PID2,
			BlackId: m.PID1,
			MatchId: m.ID,
		}
		if err := db.Create(&g).Error; err != nil {
			return err
		}
		go func() {
			if err := run(db, g); err != nil {
				log.Println(err)
			}
		}()
	}

	if len(games) < policyNumGames {
		// Create enough games
	}

	return nil
}

func run(db *gorm.DB, g api.Game) error {

	var white api.Player
	if err := db.First(&white, g.WhiteId).Error; err != nil {
		return err
	}
	var black api.Player
	if err := db.First(&black, g.BlackId).Error; err != nil {
		return err
	}

	whiteAgent := operator.RemotePlayerInstance{
		APIPlayer: white,
		Host:      white.Host,
	}
	blackAgent := operator.RemotePlayerInstance{
		APIPlayer: black,
		Host:      black.Host,
	}
	result := operator.ExecuteGame(&whiteAgent, &blackAgent, operator.Config{
		Start: game.Standard,
		Limit: api.DefaultMoveLimit,
		GameHadState: func(s *game.State) error {
			//get count
			var count int
			err := db.Table("records").Where("game_id = ?", g.ID).Count(&count).Error
			if err != nil {
				return err
			}
			//jsonify state
			var buf bytes.Buffer
			if err := json.NewEncoder(&buf).Encode(s); err != nil {
				return err
			}

			////make and save record
			r := api.Record{
				GameID:  g.ID,
				TurnNum: int64(count + 1),
				State:   buf.String(),
			}
			if err := db.Create(&r).Error; err != nil {
				return err
			}
			return nil
		},
	})

	var status api.GameStatus
	switch result.Outcome {
	case game.WhiteWins:
		status = api.GameWhiteWins
	case game.BlackWins:
		status = api.GameBlackWins
	case game.Tie:
		status = api.GameDraw
	default:
		return fmt.Errorf("unhandled case. TODO %s", result.Outcome)
	}

	if err := db.First(new(api.Game), g.ID).Update("status", status.String()).Error; err != nil {
		return err
	}
	return nil
}
