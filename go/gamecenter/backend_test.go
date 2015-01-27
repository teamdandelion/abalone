package gamecenter

import "testing"

func TestMemBackendListGames(t *testing.T) {
	b := NewMemBackend()
	b.CreateGame(Game{})
	if len(b.ListGames(0, 100)) != 1 {
		t.Fail()
	}
}
