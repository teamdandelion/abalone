package api

type Match struct {
	ID int64 `gorm:"column:id"`

	PID1 int64 `gorm:"column:player_1_id"`
	PID2 int64 `gorm:"column:player_2_id"`

	CommonDBFields
}
