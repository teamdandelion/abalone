package handlers

import (
	"encoding/json"
	"net/http"
	"strconv"

	"github.com/danmane/abalone/go/api"
)

// CreatePlayersHandler creates a new AI player running on a remote host
func CreatePlayersHandler(ds *api.Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		file, _, err := r.FormFile("exe")
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		defer file.Close()

		authorID, err := strconv.ParseInt(r.FormValue("author_id"), 10, 64)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}

		version, err := strconv.ParseInt(r.FormValue("version"), 10, 64)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		name := r.FormValue("name")
		player := api.Player{
			Name:     name,
			Version:  version,
			AuthorId: authorID,
		}

		created, err := ds.Players.Upload(authorID, player, file)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		if err := json.NewEncoder(w).Encode(&created); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	}
}

// ListPlayersHandler returns a list of AI players
func ListPlayersHandler(ds *api.Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		players, err := ds.Players.List()
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		if players == nil {
			players = make([]api.Player, 0)
		}
		if err := json.NewEncoder(w).Encode(&players); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	}
}

// DeletePlayersHandler deletes a player by id
func DeletePlayersHandler(ds *api.Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		id, err := getID(r)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}

		if err := ds.Players.Delete(id); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	}
}
