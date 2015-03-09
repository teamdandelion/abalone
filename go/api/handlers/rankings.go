package handlers

import (
	"encoding/json"
	"net/http"

	"github.com/danmane/abalone/go/api"
)

func ListRankingsHandler(ds *api.Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		rankings := []struct {
			Rank   int     `json:"rank"`
			Player string  `json:"player"`
			Author string  `json:"author"`
			Rating float64 `json:"rating"`
			Wins   int     `json:"wins"`
			Losses int     `json:"losses"`
		}{
			{1, "sumo", "btc", 2300, 40, 1},
			{2, "sidious", "danmane", 2104, 1, 40},
		}
		if err := json.NewEncoder(w).Encode(&rankings); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	}
}
