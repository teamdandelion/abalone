package handlers

import (
	"encoding/json"
	"net/http"

	api "github.com/danmane/abalone/go/api"
)

// CreateUsersHandler creates a new AI player running on a remote host
func CreateUsersHandler(ds *api.Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		var u api.User
		if err := json.NewDecoder(r.Body).Decode(&u); err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		created, err := ds.Users.Create(u)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		if err := json.NewEncoder(w).Encode(&created); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	}
}

// ListUsersHandler creates a new AI player running on a remote host
func ListUsersHandler(ds *api.Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		users, err := ds.Users.List()
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		if err := json.NewEncoder(w).Encode(&users); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	}
}

// DeleteUsersHandler creates a new AI player running on a remote host
func DeleteUsersHandler(ds *api.Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		id, err := getID(r)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}

		if err := ds.Users.Delete(id); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	}
}
