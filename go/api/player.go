package api

// Player is an AI agent running on an HTTP server
type Player struct {
	Name    string
	Version int64
	Path    string

	ID int64 `gorm:"column:id"`

	AuthorId int64 `gorm:"column:user_id"`

	CommonDBFields
}
