package db

import (
	"github.com/BurntSushi/migration"
	api "github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/api/migrations"
	"github.com/jinzhu/gorm"
)

func Open(dialect string, addr string) (*api.Services, error) {
	conn, err := migration.Open(dialect, addr, migrations.Migrations)
	if err != nil {
		return nil, err
	}
	db, err := gorm.Open(dialect, conn)
	if err != nil {
		return nil, err
	}

	return &api.Services{
		Matches: &matchesDB{&db},
		Players: &playersDB{&db},
		Users:   &usersDB{&db},
		DB:      &db,
	}, nil
}
