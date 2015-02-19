package router

import "github.com/gorilla/mux"

const (
	Players       = "players"
	PlayersCreate = "players.create"

	Games       = "games"
	GamesCreate = "games.create"
	GamesRun    = "games.run"
	Game        = "game"
	GameUpdate  = "game.update"

	APIBaseRoute = "api"
)

func NewAPIRouter() *mux.Router {
	r := mux.NewRouter()
	api := r.PathPrefix("/api").Subrouter()

	api.Path("/players").Methods("GET").Name(Players)
	api.Path("/players").Methods("POST").Name(PlayersCreate)

	api.Path("/games").Methods("GET").Name(Games)
	api.Path("/games").Methods("POST").Name(GamesCreate)
	api.Path("/games/run").Methods("POST").Name(GamesRun)
	game := api.PathPrefix("/games/{id:[0-9]+}").Subrouter()
	game.Methods("GET").Name(Game)
	game.Methods("PATCH").Name(GameUpdate) // e.g. PATCH /api/games/{GAME_ID}?state=n

	api.NewRoute().Name(APIBaseRoute) // must be declared last. used as fallback when no other routes match
	return r
}
