package gamecenter

import (
	"bytes"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestShowGameHandler(t *testing.T) {
	b := NewMemBackend()
	b.CreateGame(Game{})
	b.CreateGame(Game{})
	r, err := http.NewRequest("GET", "/api/v0/games/1", nil)
	if err != nil {
		t.Fatal(err)
	}
	w := httptest.NewRecorder()
	Router(b).ServeHTTP(w, r)

	if w.Code != http.StatusOK {
		t.Fatal(w.Code)
	}
}

func TestCreateGameHandler(t *testing.T) {
	g := &Game{
		White: Player{Name: "brian"},
		Black: Player{Name: "dan"},
	}

	data, err := json.Marshal(g)
	if err != nil {
		t.Fatal(err)
	}
	r, err := http.NewRequest("POST", "/api/v0/games", bytes.NewReader(data))
	if err != nil {
		t.Fatal(err)
	}
	w := httptest.NewRecorder()
	Router(NewMemBackend()).ServeHTTP(w, r)

	if w.Code != http.StatusOK {
		t.Fatal(w.Code)
	}

	d := json.NewDecoder(w.Body)
	var out Game
	d.Decode(&out)
	if out.Id != 0 {
		t.Fail()
	}
}

func TestListGamesHandler(t *testing.T) {
	r, err := http.NewRequest("GET", "/api/v0/games", nil)
	if err != nil {
		t.Fatal(err)
	}
	w := httptest.NewRecorder()
	Router(NewMemBackend()).ServeHTTP(w, r)

	if w.Code != http.StatusOK {
		t.Log(string(w.Body.Bytes()))
		t.Fatal(w.Code)
	}
}
