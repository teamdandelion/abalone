package router

import "github.com/gorilla/mux"

const (
	PlayersGetRoute    = "players"
	PlayersCreateRoute = "players.create"
	APIBaseRoute       = "api"
)

func NewAPIRouter() *mux.Router {
	r := mux.NewRouter()
	api := r.PathPrefix("/api").Subrouter()
	api.Path("/players").Methods("GET").Name(PlayersGetRoute)
	api.Path("/players").Methods("POST").Name(PlayersCreateRoute)
	api.NewRoute().Name(APIBaseRoute) // must be declared last. used as fallback when no other routes match
	return r
}
