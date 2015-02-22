package main

import (
	"flag"
	"log"
	"net/http"

	"github.com/codegangsta/negroni"
	api "github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/api/datastore"
	"github.com/danmane/abalone/go/api/handlers"
	"github.com/danmane/abalone/go/api/router"
	"github.com/gorilla/mux"
	_ "github.com/lib/pq"
	_ "github.com/mattn/go-sqlite3"
)

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	var (
		debug      = flag.Bool("debug", false, "")
		staticPath = flag.String("static", "./static", "serve static files located in this directory")
		host       = flag.String("host", ":8080", "address:port for HTTP listener")
		dialect    = flag.String("dialect", "postgres", "")
		dbaddr     = flag.String("db", "postgres://postgres:password@localhost/abalone?sslmode=disable", "")
	)
	flag.Parse()

	ds, err := datastore.Open(*dialect, *dbaddr)
	if err != nil {
		return err
	}
	ds.DB.LogMode(*debug)

	r := ConfigureRouter(ds, *staticPath)

	log.Printf("listening at %s", *host)
	log.Fatal(http.ListenAndServe(*host, r))
	return nil
}

func ConfigureRouter(s *api.Services, staticpath string) *mux.Router {
	r := router.NewAPIRouter()
	handlers.MountHandlers(r, s)

	// Finally, if none of the above routes match, delegate to the single-page
	// app's client-side router. Rewrite the path in order to load the
	// single-page app's root HTML entrypoint. The app will handle the route.
	r.NotFoundHandler = StaticPathFallback(staticpath)
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
