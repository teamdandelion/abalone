package gamecenter

import (
	"encoding/json"
	"log"
	"net/http"
	"strconv"

	"github.com/gorilla/mux"
)

const (
	kDefaultLimit  = 100
	kDefaultOffset = 0
)

func Router(b GamesBackend) *mux.Router {
	r := mux.NewRouter()
	apiV0 := r.PathPrefix("/api/v0").Subrouter()

	games := apiV0.Path("/games").Subrouter()
	games.Methods("GET").HandlerFunc(ListGamesHandler(b))
	games.Methods("POST").HandlerFunc(CreateGameHandler(b))

	game := apiV0.PathPrefix("/games/{id:[0-9]+}").Subrouter()
	game.Methods("GET").HandlerFunc(ShowGameHandler(b))

	// PATCH api/v0/games/{GAME_ID}?state=n :lock:
	// updates the nth game state of GAME_ID
	game.Methods("PATCH").HandlerFunc(CreateGameStateHandler(b))
	return r
}

// GET api/v0/games
// paginated list of Games: $HOST/api/v0/games?limit=10&offset=0
// filter by player: $HOST/api/v0/previous_games?player=danbot
func ListGamesHandler(b GamesBackend) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if err := r.ParseForm(); err != nil {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		log.Println(r.Form)

		var err error

		limit, err := parseIntOrDefaultOrErr(r.Form.Get("limit"), kDefaultLimit)
		if err != nil {
			w.WriteHeader(http.StatusBadRequest)
			w.Write([]byte(err.Error()))
			return
		}

		offset, err := parseIntOrDefaultOrErr(r.Form.Get("offset"), kDefaultOffset)
		if err != nil {
			w.WriteHeader(http.StatusBadRequest)
			w.Write([]byte(err.Error()))
			return
		}

		games := b.ListGames(offset, limit)
		out, err := json.Marshal(games)
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			w.Write([]byte(err.Error()))
			return
		}
		w.Write(out)
	}
}

// POST api/v0/games
// TODO :lock:
// creates a new game with two players. returns a Game (with ID)
func CreateGameHandler(b GamesBackend) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		var g Game
		decoder := json.NewDecoder(r.Body)
		err := decoder.Decode(&g)
		if err != nil {
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		created, err := b.CreateGame(g)
		if err != nil {
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		out, err := json.Marshal(created)
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		w.Write(out)
	}
}

// TODO paginated list of GameStates: $HOST/api/v0/previous_games/{GAME_ID}?limit=10&offset=0
// TODO get the nth GameState (zero-indexed): $HOST/api/v0/previous_games/{GAME_ID}?limit=1&offset=n
func ShowGameHandler(b GamesBackend) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		vars := mux.Vars(r)
		idStr, ok := vars["id"]
		if !ok {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		id, err := strconv.Atoi(idStr)
		if err != nil {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		g, err := b.FindGame(id)
		if err != nil {
			w.WriteHeader(http.StatusNotFound)
			return
		}
		out, err := json.Marshal(g)
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			w.Write([]byte(err.Error()))
			return
		}
		w.Write(out)
	}
}

func CreateGameStateHandler(b GamesBackend) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusNotImplemented)
		w.Write([]byte("TODO"))
	}
}

func parseIntOrDefaultOrErr(s string, defaultValue int) (int, error) {
	if s == "" {
		return defaultValue, nil
	} else {
		value, err := strconv.Atoi(s)
		if err != nil {
			return 0, err
		}
		return value, nil
	}
}
