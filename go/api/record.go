package api

type Record struct {
	GameID int64 `gorm:"column:game_id"`
	TurnNum int64 `gorm:"column:turn_num"`

	State string `gorm:"column:state"`

	CommonDBFields
}
