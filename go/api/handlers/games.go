package handlers

import (
	"encoding/json"
	"net/http"

	api "github.com/danmane/abalone/go/api"
)

func ListGamesHandler(ds *api.Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		games, err := ds.Games.List()
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		if games == nil {
			games = make([]api.Game, 0)
		}
		if err := json.NewEncoder(w).Encode(&games); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	}
}
