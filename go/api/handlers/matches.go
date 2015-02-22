package handlers

import (
	"encoding/json"
	"net/http"

	api "github.com/danmane/abalone/go/api"
	// "github.com/danmane/abalone/go/game"
	// "github.com/danmane/abalone/go/operator"
)

// RunMatchesHandler schedules a match between two players
func RunMatchesHandler(ds *api.Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		var matchrequest api.Match
		if err := json.NewDecoder(r.Body).Decode(&matchrequest); err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		match, err := ds.Matches.Run(matchrequest.PID1, matchrequest.PID2)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		if err := json.NewEncoder(w).Encode(match); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	}
}
