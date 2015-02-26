package api

import "github.com/danmane/abalone/go/game"

type Game struct {
	ID int64 `gorm:"column:id"`

	MatchId int64 `gorm:"column:match_id"`
	WhiteId int64 `gorm:"column:white_player_id"`
	BlackId int64 `gorm:"column:black_player_id"`

	Reason string `gorm:"column:reason"`
	Status string `gorm:"column:status"`

	CommonDBFields
}

type GameWithDetails struct {
	ID int64
	
	MatchId int64 `gorm:"column:match_id"`
	WhitePlayer Player 
	WhiteID int64 `gorm:"column:white_player_id"`
	BlackPlayer Player
	BlackID int64 `gorm:"column:black_player_id"`
	
	Status string `gorm:"column:status"`
	CommonDBFields
	Reason string `gorm:"column:reason"`
}

func (c GameWithDetails) TableName() string {
    return "games"
}

type GameStatus string

const (
	GameScheduled GameStatus = "scheduled"
	GameDraw      GameStatus = "draw"
	GameBlackWins GameStatus = "black_wins"
	GameWhiteWins GameStatus = "white_wins"
)

func (s GameStatus) String() string {
	return string(s)
}

type GameResult struct {
	White         Player
	Black         Player
	Outcome       game.Outcome
	VictoryReason Victory
	States        []game.State
}
