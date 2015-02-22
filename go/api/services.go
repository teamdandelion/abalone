package api

import "github.com/jinzhu/gorm"

type Services struct {
	Players PlayersService
	Users   UsersService

	DB *gorm.DB
}

type PlayersService interface {
	Create(userID int64, p Player) (*Player, error)
	List() ([]Player, error)
	Delete(id int64) error
}

type UsersService interface {
	Create(User) (*User, error)
	List() ([]User, error)
	Delete(id int64) error
}
