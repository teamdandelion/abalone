package router

import "github.com/gorilla/mux"

const (
	Rankings = "rankings"

	Users       = "users"
	UsersCreate = "users.create"
	UsersDelete = "users.delete"

	Players       = "players"
	PlayersCreate = "players.create"
	PlayersDelete = "players.delete"

	Games       = "games"
	GamesCreate = "games.create"
	Game        = "game"
	GameUpdate  = "game.update"
	GameStates  = "game.states"

	States = "states"

	MatchesRun = "matches.run"

	APIBaseRoute = "api"
)

func NewAPIRouter() *mux.Router {
	r := mux.NewRouter()
	api := r.PathPrefix("/api").Subrouter()

	api.Path("/rankings").Methods("GET").Name(Rankings)

	api.Path("/matches/run").Methods("POST").Name(MatchesRun)

	api.Path("/users").Methods("GET").Name(Users)
	api.Path("/users").Methods("POST").Name(UsersCreate)
	api.Path("/users/{id:[0-9]+}").Methods("DELETE").Name(UsersDelete)

	api.Path("/players").Methods("GET").Name(Players)
	api.Path("/players").Methods("POST").Name(PlayersCreate)
	api.Path("/players/{id:[0-9]+}").Methods("DELETE").Name(PlayersDelete)

	api.Path("/games/{game_id:[0-9]+}/states").Methods("GET").Name(GameStates) // this more specific route must appear before the Game subrouter
	api.Path("/states").Methods("GET").Name(States)                            // short alias provided for convenience

	api.Path("/games").Methods("GET").Name(Games)
	api.Path("/games").Methods("POST").Name(GamesCreate)
	game := api.PathPrefix("/games/{id:[0-9]+}").Subrouter()
	game.Methods("GET").Name(Game)
	game.Methods("PATCH").Name(GameUpdate) // e.g. PATCH /api/games/{GAME_ID}?state=n

	api.NewRoute().Name(APIBaseRoute) // must be declared last. used as fallback when no other routes match
	return r
}
