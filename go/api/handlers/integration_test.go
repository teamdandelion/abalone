package handlers

import (
	"flag"
	"net/http/httptest"

	"testing"

	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/api/client"
	db "github.com/danmane/abalone/go/api/db"
	"github.com/danmane/abalone/go/api/router"
	_ "github.com/lib/pq"
)

const (
	dialect = "postgres"
)

var pgaddr = flag.String("pgaddr", "", "postgres db address spec")

func NewTestServer(t *testing.T) *httptest.Server {
	if *pgaddr == "" {
		t.SkipNow()
	}
	ds, err := db.Open(dialect, *pgaddr)
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
