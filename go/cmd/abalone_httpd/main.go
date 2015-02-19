package main

import (
	"database/sql"
	"encoding/json"
	"flag"
	"log"
	"net/http"

	"github.com/codegangsta/negroni"
	api "github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/game"
	"github.com/danmane/abalone/go/operator"
	"github.com/danmane/abalone/go/router"
	"github.com/gorilla/mux"
	_ "github.com/mattn/go-sqlite3"
)

var (
	staticPath = flag.String("static", "./static", "serve static files located in this directory")
	host       = flag.String("host", ":8080", "address:port for HTTP listener")
)

func main() {
	flag.Parse()
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {

	conn, err := sql.Open("sqlite3", "sqlite.db")
	if err != nil {
		return err
	}
	ds, err := NewDatastore(conn, "sqlite3")
	if err != nil {
		return err
	}

	r := ConfigureRouter(ds, *staticPath)

	log.Printf("listening at %s", *host)
	log.Fatal(http.ListenAndServe(*host, r))
	return nil
}

func ConfigureRouter(s *api.Services, staticpath string) *mux.Router {
	r := router.NewAPIRouter()
	MountHandlers(r, s)

	// Finally, if none of the above routes match, delegate to the single-page
	// app's client-side router. Rewrite the path in order to load the
	// single-page app's root HTML entrypoint. The app will handle the route.
	r.NotFoundHandler = StaticPathFallback(staticpath)
	return r
}

func MountHandlers(r *mux.Router, ds *api.Services) *mux.Router {
	r.Get(router.GamesRun).HandlerFunc(RunGamesHandler(ds))
	r.Get(router.Players).HandlerFunc(ListPlayersHandler(ds))
	r.Get(router.PlayersCreate).HandlerFunc(CreatePlayersHandler(ds))
	r.Get(router.APIBaseRoute).Path("/{rest:.*}").HandlerFunc(http.NotFound)
	return r
}

func StaticPathFallback(path string) http.Handler {
	return negroni.New(
		negroni.NewStatic(http.Dir(path)),
		negroni.Wrap(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			r.URL.Path = "/"
			http.FileServer(http.Dir(path)).ServeHTTP(w, r)
		})))
}

// RunGamesHandler runs a game between two remote player instances
func RunGamesHandler(ds *api.Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		req := struct {
			BlackPort string
			WhitePort string
		}{}
		if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		whiteAgent := operator.RemotePlayerInstance{
			APIPlayer: api.Player{},
			Port:      req.WhitePort,
		}
		blackAgent := operator.RemotePlayerInstance{
			APIPlayer: api.Player{},
			Port:      req.BlackPort,
		}
		result := operator.ExecuteGame(&whiteAgent, &blackAgent, operator.Config{
			Start: game.Standard,
			Limit: api.DefaultMoveLimit,
		})
		log.Println(result.Outcome)
		log.Println(result.VictoryReason)
		if err := json.NewEncoder(w).Encode(result); err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
	}
}

// CreatePlayersHandler creates a new AI player running on a remote host
func CreatePlayersHandler(ds *api.Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		// schema := struct {
		// 	Nick    string
		// 	Version string
		// 	Address string
		// }{}
		// if err := json.NewDecoder(r.Body).Decode(&schema); err != nil {
		// 	w.WriteHeader(http.StatusBadRequest)
		// 	fmt.Fprintf(w, "error decoding request: %s", err)
		// 	return
		// }
	}
}

// ListPlayersHandler returns a list of AI players
func ListPlayersHandler(ds *api.Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusNotImplemented)
	}
}
