package handlers

import (
	"net/http/httptest"

	"testing"

	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/api/client"
	"github.com/danmane/abalone/go/api/datastore"
	"github.com/danmane/abalone/go/api/router"
	_ "github.com/lib/pq"
)

func NewTestServer(t *testing.T) *httptest.Server {
	const (
		dialect = "postgres"
		addr    = "postgres://postgres:password@localhost/abalonetest?sslmode=disable"
	)
	ds, err := datastore.Open(dialect, addr)
	if err != nil {
		t.Fatal(err)
	}

	r := router.NewAPIRouter()
	MountHandlers(r, ds)
	return httptest.NewServer(r)
}

func TestUsersCreate(t *testing.T) {
	s := NewTestServer(t)
	defer s.Close()
	client := client.NewClient(client.BaseURL(s.URL))
	_, err := client.Users.Create(api.User{Name: "brian", Email: "brian.holderchow@gmail.com"})
	if err != nil {
		t.Fatal(err)
	}
}
