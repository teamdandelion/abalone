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

func translate(dialect string) string {
	q := `
	CREATE TABLE authors (
		AUTOINCREMENTING_PRIMARYKEY_INTEGER_ID,
		name VARCHAR(40) NOT NULL
	);

	CREATE TABLE players (
		AUTOINCREMENTING_PRIMARYKEY_INTEGER_ID,
		name VARCHAR(100) NOT NULL,
		version INT NOT NULL,
		author_id INT NOT NULL, 
		FOREIGN KEY (author_id) REFERENCES authors (id)
	);

	CREATE TABLE ref_outcome (
		AUTOINCREMENTING_PRIMARYKEY_INTEGER_ID,
		name VARCHAR(25) NOT NULL
	);

	CREATE TABLE games (
		AUTOINCREMENTING_PRIMARYKEY_INTEGER_ID,
		first_player INT NOT NULL,
		second_player INT NOT NULL,
		outcome INT NULL,
		winner INT NULL,
		match_id INT NOT NULL,
		FOREIGN KEY (first_player) REFERENCES players (id),
		FOREIGN KEY (second_player) REFERENCES players (id),
		FOREIGN KEY (outcome_id) REFERENCES ref_outcome (id),
		FOREIGN KEY (winner) REFERENCES players (id),
		FOREIGN KEY (match_id) REFERENCES matches (id)
	);

	CREATE TABLE records (
		game_id INT NOT NULL,
		turn_num INT NOT NULL,
		state JSON NOT NULL,
		FOREIGN KEY (game_id) REFERENCES games (id),
		PRIMARY KEY (game_id, turn_num)
	);

	CREATE TABLE matches (
		AUTOINCREMENTING_PRIMARYKEY_INTEGER_ID,
		players[] INT[] NOT NULL
	);
	`

	var statement string
	switch (dialect){
	case "postgres":
		statement = "id SERIAL PRIMARY KEY"
	case "sqlite3":
		statement = "id INTEGER PRIMARY KEY"
	case "mysql":
		statement = `id INTEGER NOT NULL AUTO_INCREMENT,
		PRIMARY KEY (id)`
	}
	return strings.Replace(q,"AUTOINCREMENTING_PRIMARYKEY_INTEGER_ID", statement, -1)
}

func run() error {
	sql, err := gorm.Open(*dbOption, *dbAddr)
	if err != nil {
		return err
	}


	return sql.Exec(constructed_q).Error
}
