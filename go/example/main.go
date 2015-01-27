package main

import (
	"encoding/json"
	"flag"
	"log"
	"net/http"

	"github.com/gorilla/mux"
)

var host = flag.String("host", ":3423", "host address:port")

type AgentInfo struct {
	Owner  string
	Taunts []string
}

func main() {
	r := mux.NewRouter()
	r.Path("/ping").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		agent := &AgentInfo{
			Owner: "btc",
			Taunts: []string{
				"U MAD BRO?",
				"If you took an IQ test, the results would be negative.",
				"I don't know what makes you so dumb but it really works",
				"If brains were taxed, you'd get a rebate.",
				"Zombies eat brains. You’re safe.",
			},
		}
		w.WriteHeader(http.StatusOK)
		json.NewEncoder(w).Encode(agent)
	})

	r.Path("/move").HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// TODO extract game state from request body
		// TODO write next game state to response writer
		w.WriteHeader(http.StatusOK)
	})
	log.Fatal(http.ListenAndServe(*host, r))
}
