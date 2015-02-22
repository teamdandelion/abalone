package router

import "github.com/gorilla/mux"

const (
	Users       = "users"
	UsersCreate = "users.create"
	UsersDelete = "users.delete"

	Players       = "players"
	PlayersCreate = "players.create"
	PlayersDelete = "players.delete"

	Games       = "games"
	GamesRun    = "games.run"
	GamesCreate = "games.create"
	Game        = "game"
	GameUpdate  = "game.update"

	MatchesRun = "matches.run"

	APIBaseRoute = "api"
)

func NewAPIRouter() *mux.Router {
	r := mux.NewRouter()
	api := r.PathPrefix("/api").Subrouter()

	api.Path("/users").Methods("GET").Name(Users)
	api.Path("/users").Methods("POST").Name(UsersCreate)
	api.Path("/users/{id:[0-9]+}").Methods("DELETE").Name(UsersDelete)

	api.Path("/players").Methods("GET").Name(Players)
	api.Path("/players").Methods("POST").Name(PlayersCreate)
	api.Path("/players/{id:[0-9]+}").Methods("DELETE").Name(PlayersDelete)

	api.Path("/games").Methods("GET").Name(Games)
	api.Path("/games/run").Methods("POST").Name(GamesRun)
	api.Path("/games").Methods("POST").Name(GamesCreate)
	game := api.PathPrefix("/games/{id:[0-9]+}").Subrouter()
	game.Methods("GET").Name(Game)
	game.Methods("PATCH").Name(GameUpdate) // e.g. PATCH /api/games/{GAME_ID}?state=n

	api.NewRoute().Name(APIBaseRoute) // must be declared last. used as fallback when no other routes match
	return r
}
