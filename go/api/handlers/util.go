package handlers

import (
	"net/http"
	"strconv"

	"github.com/danmane/abalone/go/api"
	routes "github.com/danmane/abalone/go/api/router"
	"github.com/gorilla/mux"
)

func MountHandlers(r *mux.Router, ds *api.Services) {
	r.Get(routes.MatchesRun).HandlerFunc(RunMatchesHandler(ds))

	r.Get(routes.Players).HandlerFunc(ListPlayersHandler(ds))
	r.Get(routes.PlayersCreate).HandlerFunc(CreatePlayersHandler(ds))
	r.Get(routes.PlayersDelete).HandlerFunc(DeletePlayersHandler(ds))

	r.Get(routes.Users).HandlerFunc(ListUsersHandler(ds))
	r.Get(routes.UsersCreate).HandlerFunc(CreateUsersHandler(ds))
	r.Get(routes.UsersDelete).HandlerFunc(DeleteUsersHandler(ds))

	r.Get(routes.APIBaseRoute).Path("/{rest:.*}").HandlerFunc(http.NotFound)
}

func getID(r *http.Request) (int64, error) {
	return strconv.ParseInt(mux.Vars(r)["id"], 10, 64)
}
