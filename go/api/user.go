package api

type User struct {
	Name  string
	Email string

	ID int64 `gorm:"column:id"`

	CommonDBFields
}
