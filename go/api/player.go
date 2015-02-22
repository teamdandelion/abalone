package api

// Player is an AI agent running on an HTTP server
type Player struct {
	Name    string
	Version int64
	Host    string

	ID int64 `gorm:"column:id"`

	Author   User
	AuthorId int64

	CommonDBFields
}
