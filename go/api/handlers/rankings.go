package handlers

import (
	"encoding/json"
	"net/http"

	"github.com/danmane/abalone/go/api"
)

func ListRankingsHandler(ds *api.Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		rankings, err := ds.Rankings.List()
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		if err := json.NewEncoder(w).Encode(&rankings); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	}
}
