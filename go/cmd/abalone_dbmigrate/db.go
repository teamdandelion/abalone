package main

import (
	"github.com/danmane/abalone/go/api"
	"github.com/jinzhu/gorm"
)

// AutoMigrate ensures all tables and columns are up to date (non-destructive)
func AutoMigrate(sql *gorm.DB) *gorm.DB {
	return sql.AutoMigrate(
		&api.Author{},
		&api.Player{},
		&api.StartState{},
		&api.Game{},
		&api.Participant{},
		&api.Record{},
	)
}
