package handlers

import (
	"encoding/json"
	"fmt"
	"net/http"
	"strconv"

	"github.com/danmane/abalone/go/api"
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
		games, err := ds.Games.ListDetailed()
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		if games == nil {
			games = make([]*api.GameWithDetails, 0)
		}
		if err := json.NewEncoder(w).Encode(&games); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	}
}

type GameStatesAPIResponse struct {
	White         *string
	Black         *string
	Outcome       string
	VictoryReason string
	States        []string `sql:"-"`
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
		var game GameStatesAPIResponse
		var records []api.Record
		if err := ds.DB.Where(api.Record{GameID: gameID}).Order("turn_num asc").Find(&records).Pluck("state", &game.States).Error; err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}

		if err := ds.DB.Raw(` SELECT 
			ROW_TO_JSON(whiteplayer) AS White,
			ROW_TO_JSON(blackplayer) AS Black,
    		g.status AS Outcome,
    		g.reason AS VictoryReason
    		FROM games g
			LEFT JOIN players whiteplayer ON whiteplayer.id = g.white_player_id
			LEFT JOIN players blackplayer ON blackplayer.id = g.black_player_id
			WHERE (g.id = ?`, gameID).Scan(&game).Error; err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}

		if err := json.NewEncoder(w).Encode(game); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	}
}
