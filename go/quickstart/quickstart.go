package quickstart

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"

	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/game"
	"github.com/gorilla/mux"
)

func Play(player api.Player, f func(game.State) game.State) { // TODO pointers not copies
	r := mux.NewRouter()
	r.Path("/ping").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		json.NewEncoder(w).Encode(&player)
	})
	r.Path("/move").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		var s game.State
		if err := json.NewDecoder(r.Body).Decode(&s); err != nil {
			w.WriteHeader(http.StatusBadRequest)
			fmt.Fprintf(w, "error decoding game state from request body")
			return
		}

		next := f(s)

		w.WriteHeader(http.StatusOK)
		json.NewEncoder(w).Encode(next)
	})
	log.Fatal(http.ListenAndServe(player.Address, r))
}
