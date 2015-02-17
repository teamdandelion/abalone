package main

import (
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"os"
	"path"
	"testing"
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
	router := Router(nil, dir)
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
	dir := makeStaticPath(t)
	router := Router(nil, dir)
	w := httptest.NewRecorder()
	r, err := http.NewRequest("GET", "/api/v0/foo", nil)
	if err != nil {
		t.Fatal(err)
	}
	router.ServeHTTP(w, r)

	if w.Code != http.StatusNotFound {
		t.Fatalf("expected not found, received", w.Code)
	}
}

func TestRouterAPINormalMatch(t *testing.T) {
	path := "/api/v0/players"
	dir := makeStaticPath(t)

	s := &Services{}
	router := Router(s, dir)
	r, err := http.NewRequest("GET", path, nil)
	if err != nil {
		t.Fatal(err)
	}
	w := httptest.NewRecorder()
	router.ServeHTTP(w, r)
	switch w.Code {
	case http.StatusNotFound:
		t.Fatal("should have found route")
	case http.StatusNotImplemented:
		// expected
	default:
		t.Fatal("expected %s, got %s", http.StatusNotImplemented, w.Code)
	}
}
