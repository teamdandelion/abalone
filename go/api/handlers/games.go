package handlers

import (
	"encoding/json"
	"fmt"
	"net/http"
	"strconv"
	"strings"

	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/game"
	"github.com/gorilla/mux"
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

func ListDetailsGamesHandler(ds *api.Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		games, err := ds.Games.ListDetailled()
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		if games == nil {
			games = make([]api.GameWithDetails, 0)
		}
		if err := json.NewEncoder(w).Encode(&games); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	}
}

func ListGameStatesHandler(ds *api.Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		gidStr := mux.Vars(r)["game_id"]
		if gidStr == "" {
			// if wasn't in mux path, try URL query param
			gidStr = r.URL.Query().Get("game_id")
		}
		if gidStr == "" {
			http.Error(w, "game_id is required", http.StatusBadRequest)
			return
		}
		gameID, err := strconv.ParseInt(gidStr, 10, 64)
		if err != nil {
			http.Error(w, fmt.Sprintf("error parsing game_id: %s", err.Error()), http.StatusBadRequest)
			return
		}

		var records []api.Record
		if err := ds.DB.Where(api.Record{GameID: gameID}).Order("turn_num asc").Find(&records).Error; err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		states := make([]*game.State, len(records), len(records))
		for i, r := range records {
			var s game.State
			if err := json.NewDecoder(strings.NewReader(r.State)).Decode(&s); err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
				return
			}
			states[i] = &s
		}
		if err := json.NewEncoder(w).Encode(states); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	}
}


