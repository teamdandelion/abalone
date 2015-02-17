package router

import "testing"

func TestAPIBaseRoute(t *testing.T) {
	r := NewAPIRouter()
	if url, _ := r.Get(PlayersGetRoute).URL(); url.Path != "/api/players" {
		t.Fatal("unexpected")
	}
}
