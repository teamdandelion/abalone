package db

import (
	"github.com/BurntSushi/migration"
	api "github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/api/migrations"
	"github.com/danmane/abalone/go/operator"
	"github.com/jinzhu/gorm"
)

func Open(dialect string, addr string, filestoragePath string) (*api.Services, error) {
	conn, err := migration.Open(dialect, addr, migrations.Migrations)
	if err != nil {
		return nil, err
	}
	db, err := gorm.Open(dialect, conn)
	if err != nil {
		return nil, err
	}

	r := &resources{
		DB:              &db,
		Ports:           operator.NewScheduler(16000, 2000),
		FilestoragePath: filestoragePath,
	}
	s := &api.Services{
		Rankings: &rankingsDB{r},
		Games:    &gamesDB{r},
		Matches:  &matchesDB{r},
		Players:  &playersDB{r},
		Users:    &usersDB{r},
		DB:       &db,
	}
	r.Services = s
	return s, nil
}

// resources bundles up all of the components required by the various DB
// structs.
type resources struct {
	DB              *gorm.DB
	Ports           *operator.PortScheduler
	FilestoragePath string
	Services        *api.Services
}
