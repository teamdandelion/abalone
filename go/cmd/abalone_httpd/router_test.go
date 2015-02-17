package main

import (
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"os"
	"path"
	"testing"

	"github.com/fsouza/go-dockerclient"
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
	path := "/api/v0/agents"
	dir := makeStaticPath(t)

	// NB: Provided tcp url is not routable. That's okay. We just need to pass
	// a valid value (any valid value will do). This test is merely to ensure
	// that we can route to |path|. Doesn't matter whether the request can be
	// handled by the AgentSupervisor.
	c, err := docker.NewClient("tcp://0.0.0.0:2376")
	if err != nil {
		t.Fatal(err)
	}
	s := &AgentSupervisor{Client: c}
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
	case http.StatusInternalServerError:
		// expected
	default:
		t.Fatal("expected internal error (because we passed an invalid AgentSupervisor")
	}
}
