package main

import (
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"os"
	"path"
	"testing"

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
