package quickstart

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"time"

	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/game"
	"github.com/gorilla/mux"
)

func Run(port string, f func(game.State, time.Duration) game.State) { // TODO pointers not copies
	r := mux.NewRouter()
	r.Path(api.PingPath).HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
	})
	r.Path(api.MovePath).HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		var mr api.MoveRequest
		if err := json.NewDecoder(r.Body).Decode(&mr); err != nil {
			w.WriteHeader(http.StatusBadRequest)
			fmt.Fprintf(w, "error decoding game state from request body")
			return
		}

		limit := time.Duration(mr.LimitMilli) * time.Millisecond

		next := f(mr.State, limit)

		w.WriteHeader(http.StatusOK)
		json.NewEncoder(w).Encode(next)
	})
	log.Fatal(http.ListenAndServe("localhost:"+port, r))
}
