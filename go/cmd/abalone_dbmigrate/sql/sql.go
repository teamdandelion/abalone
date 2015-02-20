package main

import (
	"log"

	"github.com/jinzhu/gorm"
	_ "github.com/lib/pq"
	_ "github.com/mattn/go-sqlite3"

	"flag"
)

var (
	dbOption = flag.String("db", "sqlite3", "serve static files located in this directory")
	dbAddr   = flag.String("addr", "sqlite.db", "serve static files located in this directory")
)

func main() {
	flag.Parse()
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	sql, err := gorm.Open(*dbOption, *dbAddr)
	if err != nil {
		return err
	}
	q := `
	`
	return sql.Exec(q).Error
}
