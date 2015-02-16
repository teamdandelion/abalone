package abalone

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"

	"github.com/gorilla/mux"
)

const (
	host = ":3423"
)

type AgentInfo struct {
	Owner  string
	Taunts []string
}

func Play(info AgentInfo, f func(Game) Game) {
	r := mux.NewRouter()
	r.Path("/ping").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		json.NewEncoder(w).Encode(&info)
	})
	r.Path("/move").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		var g Game
		if err := json.NewDecoder(r.Body).Decode(g); err != nil {
			w.WriteHeader(http.StatusBadRequest)
			fmt.Fprintf(w, "error decoding game state from request body")
			return
		}

		next := f(g)

		w.WriteHeader(http.StatusOK)
		json.NewEncoder(w).Encode(next)
	})
	log.Fatal(http.ListenAndServe(host, r))
}
