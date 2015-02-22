package main

import (
	"database/sql"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"os"
	"path"
	"testing"

	api "github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/api/router"
	_ "github.com/mattn/go-sqlite3"
)

func makeStaticPath(t *testing.T) string {
	dir, err := ioutil.TempDir("", "")
	if err != nil {
		t.Fatal(err)
	}
	if err := ioutil.WriteFile(path.Join(dir, "index.html"), nil, os.ModeTemporary); err != nil {
		t.Fatal(err)
	}
	return dir
}

func TestRouteRoot(t *testing.T) {
	dir := makeStaticPath(t)
	router := ConfigureRouter(nil, dir)
	w := httptest.NewRecorder()
	r, err := http.NewRequest("GET", "/", nil)
	if err != nil {
		t.Fatal(err)
	}
	router.ServeHTTP(w, r)

	if w.Code != http.StatusOK {
		t.Fatal("expected to serve index html, but got", w.Code)
	}
}

func TestRouterAPINotFound(t *testing.T) {
	router := ConfigureRouter(nil, makeStaticPath(t))
	w := httptest.NewRecorder()
	r, err := http.NewRequest("GET", "/api/foo", nil)
	if err != nil {
		t.Fatal(err)
	}
	router.ServeHTTP(w, r)

	if w.Code != http.StatusNotFound {
		t.Fatalf("expected not found, got %s", http.StatusText(w.Code))
	}
}

func TestRouterAPINormalMatch(t *testing.T) {

	s, teardown := setup(t)
	defer teardown()
	r := ConfigureRouter(s, makeStaticPath(t))
	url, err := r.Get(router.Players).URL()
	if err != nil {
		t.Fatal(err)
	}

	req, err := http.NewRequest("GET", url.Path, nil)
	if err != nil {
		t.Fatal(err)
	}
	w := httptest.NewRecorder()
	r.ServeHTTP(w, req)
	expected := http.StatusOK
	switch w.Code {
	case http.StatusNotFound:
		t.Fatal("should have found route")
	case expected:
		// expected
	default:
		t.Fatalf("expected %s, got %s", http.StatusText(expected), http.StatusText(w.Code))
	}
}

func TestCreatePlayersHandler(t *testing.T) {
	s, teardown := setup(t)
	defer teardown()
	dir := makeStaticPath(t)
	r := ConfigureRouter(s, dir)
	url, err := r.Get(router.PlayersCreate).URL()
	if err != nil {
		t.Fatal(err)
	}

	req, err := http.NewRequest("POST", url.Path, nil)
	if err != nil {
		t.Fatal(err)
	}
	w := httptest.NewRecorder()
	r.ServeHTTP(w, req)
}

// setup creates a SQLite DB in a temporary directory. Call the teardown to
// delete the database.
func setup(t *testing.T) (services *api.Services, teardown func()) {
	name, err := ioutil.TempDir("", "")
	if err != nil {
		t.Fatal(err)
	}
	conn, err := sql.Open("sqlite3", path.Join(name, "sqlite.db"))
	if err != nil {
		t.Fatal(err)
	}
	ds, err := NewDatastore(conn, "sqlite3")
	if err != nil {
		t.Fatal(err)
	}
	if err := AutoMigrate(ds.DB); err != nil {
		t.Fatal(err)
	}
	return ds, func() {
		conn.Close()
		os.RemoveAll(name)
	}
}
