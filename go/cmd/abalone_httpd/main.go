package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net/http"

	"github.com/codegangsta/negroni"
	"github.com/gorilla/mux"
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
	log.Printf("listening at %s", *host)
	s := Services{}
	log.Fatal(http.ListenAndServe(*host, Router(&s, *staticPath)))
	return nil
}

type Services struct {
}

// Router defines URL routes
func Router(s *Services, path string) *mux.Router {
	r := mux.NewRouter()
	WireAPIRoutes(s, r)

	// Finally, if none of the above routes match, delegate to the single-page
	// app's client-side router. Rewrite the path in order to load the
	// single-page app's root HTML entrypoint. The app will handle the route.
	r.NotFoundHandler = StaticPathFallback(path)
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

func WireAPIRoutes(s *Services, r *mux.Router) {
	apiV0 := r.PathPrefix("/api/v0").Subrouter()

	apiV0.Path("/players").Methods("GET").HandlerFunc(ListPlayersHandler(s))
	apiV0.Path("/players").Methods("POST").HandlerFunc(CreatePlayersHandler(s))

	// If no API routes matched, but path had API prefix, return 404.
	apiV0.Path("/{rest:.*}").HandlerFunc(http.NotFound)
}

// CreatePlayersHandler creates a new AI player running on a remote host
func CreatePlayersHandler(s *Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		schema := struct {
			Nick    string
			Version string
			Address string
		}{}
		if err := json.NewDecoder(r.Body).Decode(&schema); err != nil {
			w.WriteHeader(http.StatusBadRequest)
			fmt.Fprintf(w, "error decoding request: %s", err)
			return
		}
	}
}

// ListPlayersHandler returns a list of AI players
func ListPlayersHandler(s *Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusNotImplemented)
	}
}
