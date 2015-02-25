package db

import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"path"

	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/game"
	"github.com/danmane/abalone/go/operator"
	"github.com/jinzhu/gorm"
)

type matchesDB struct {
	db              *gorm.DB
	scheduler       operator.PortScheduler
	filestoragePath string
}

func (s *matchesDB) Run(playerID1, playerID2 int64) (*api.Match, error) {
	matchrequest := api.Match{
		PID1: playerID1,
		PID2: playerID2,
	}
	if err := s.db.Create(&matchrequest).Error; err != nil {
		return nil, err
	}
	if err := ExecuteMatch(s, matchrequest); err != nil {
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

func ExecuteMatch(mdb *matchesDB, m api.Match) error {

	// this method is choc-full'o opportunities to mix up variables (1 and 2).
	// be careful and avoid mutable state.

	const (
		// TODO assert there are only |policyNumGames| games in the match
		policyNumGames = 2
	)

	var games []api.Game
	if err := mdb.db.Where(api.Match{ID: m.ID}).Find(&games).Error; err != nil {
		return err
	}

	// constraints represents the idea that we want to find enough games such
	// that both players had a chance to play as white

	constraints := struct {
		Player1AsWhite bool
		Player1AsBlack bool

		Player2AsWhite bool
		Player2AsBlack bool
	}{}

	for _, g := range games {
		if g.WhiteId == m.PID1 {
			constraints.Player1AsWhite = true
		}
		if g.BlackId == m.PID1 {
			constraints.Player1AsBlack = true
		}
		if g.BlackId == m.PID2 {
			constraints.Player2AsBlack = true
		}
		if g.WhiteId == m.PID2 {
			constraints.Player2AsWhite = true
		}
	}

	// this block of code checks for cases where one player played as white,
	// but the other didn't play as black (and vice-versa)
	if (constraints.Player1AsWhite != constraints.Player2AsBlack) || (constraints.Player1AsBlack != constraints.Player2AsWhite) {
		return fmt.Errorf("couldn't run games. database inconsistency detected. match: %+v, constraints: %+v", m, constraints)
	}

	// https://www.youtube.com/watch?v=zyu2jAD6sdo

	if !constraints.Player1AsWhite {
		g := api.Game{
			Status:  api.GameScheduled.String(),
			WhiteId: m.PID1,
			BlackId: m.PID2,
			MatchId: m.ID,
		}
		if err := mdb.db.Create(&g).Error; err != nil {
			return err
		}
		go func() {
			if err := run(mdb, g); err != nil {
				log.Println(err)
			}
		}()
	}

	if !constraints.Player2AsWhite {
		g := api.Game{
			Status:  api.GameScheduled.String(),
			WhiteId: m.PID2,
			BlackId: m.PID1,
			MatchId: m.ID,
		}
		if err := mdb.db.Create(&g).Error; err != nil {
			return err
		}
		go func() {
			if err := run(mdb, g); err != nil {
				log.Println(err)
			}
		}()
	}

	return nil
}

func run(matches *matchesDB, g api.Game) error {

	var white api.Player
	if err := matches.db.First(&white, g.WhiteId).Error; err != nil {
		return err
	}
	var black api.Player
	if err := matches.db.First(&black, g.BlackId).Error; err != nil {
		return err
	}
	whiteAgent, err := operator.NewPlayerProcessInstance(white, path.Join(matches.filestoragePath, white.Path), matches.scheduler)
	if err != nil {
		return err
	}
	blackAgent, err := operator.NewPlayerProcessInstance(black, path.Join(matches.filestoragePath, black.Path), matches.scheduler)
	if err != nil {
		return err
	}
	result := operator.ExecuteGame(whiteAgent, blackAgent, operator.Config{
		Start: game.Standard,
		Limit: api.DefaultMoveLimit,
		GameHadState: func(s *game.State) error {
			//get count
			var count int
			err := matches.db.Table("records").Where("game_id = ?", g.ID).Count(&count).Error
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
			if err := matches.db.Create(&r).Error; err != nil {
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
	result.VictoryReason.String()

	updates := map[string]interface{}{
		"status": status.String(),
		"reason": result.VictoryReason.String(),
	}
	if err := matches.db.First(new(api.Game), g.ID).Updates(updates).Error; err != nil {
		return err
	}
	return nil
}
