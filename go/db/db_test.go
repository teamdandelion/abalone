package db

import (
	"io/ioutil"
	"os"
	"path"
	"testing"

	"github.com/jinzhu/gorm"
	_ "github.com/mattn/go-sqlite3"
)

func setupDB(t *testing.T) (*gorm.DB, func()) {
	name, err := ioutil.TempDir("", "")
	if err != nil {
		t.Fatal(err)
	}
	sql, err := gorm.Open("sqlite3", path.Join(name, "sqlite.db"))
	if err != nil {
		t.Fatal(err)
	}
	AutoMigrate(&sql)
	return &sql, func() {
		os.RemoveAll(name)
	}
}

func TestAuthorHasManyPlayers(t *testing.T) {
	sql, teardownDB := setupDB(t)
	defer teardownDB()

	// create author with two players
	sql.Create(&Author{
		Name: "btc",
		Players: []Player{
			{Nick: "basic"},
			{Nick: "smart"},
		},
	})
	// get the players count. it should be two
	var players []Player
	sql.Find(&players)
	if len(players) != 2 {
		t.Fatal("expected two players", players)
	}
	player := players[0]
	var author Author
	sql.Model(&player).Related(&author)

	if author.Name != "btc" {
		t.Fatal("expected the player to have an associated author")
	}
}
