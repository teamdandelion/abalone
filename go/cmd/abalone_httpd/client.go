package main

import (
	"database/sql"
	"encoding/json"
	"errors"
	"net/http"

	api "github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/api/router"
	"github.com/gorilla/mux"
	"github.com/jinzhu/gorm"
)

func NewDatastore(sqldb *sql.DB, drivername string) (*api.Services, error) {

	db, err := gorm.Open(drivername, sqldb)
	if err != nil {
		return nil, err
	}

	return &api.Services{
		Players: &playersStore{&db},
		DB:      &db,
	}, nil
}

type playersStore struct {
	db *gorm.DB
}

func (s *playersStore) List() ([]api.Player, error) {
	return nil, ErrTODO
}

func (s *playersStore) Create() error {
	return ErrTODO
}

var _ api.PlayersService = &playersStore{}

type httpClient struct {
	BaseURL string

	httpClient *http.Client
	apiRouter  *mux.Router
}

type playersService struct {
	client *httpClient
}

func (s *playersService) List() ([]api.Player, error) {
	url, err := s.client.apiRouter.Get(router.Players).URL()
	if err != nil {
		return nil, err
	}
	res, err := s.client.httpClient.Get(s.client.BaseURL + url.String())
	if err != nil {
		return nil, err
	}
	defer res.Body.Close()
	var players []api.Player
	if err := json.NewDecoder(res.Body).Decode(&players); err != nil {
		return nil, err
	}
	return players, nil
}

func (s *playersService) Create() error {
	return ErrTODO
}

var _ api.PlayersService = &playersService{}

const (
	DefaultBaseURL = "http://todo.com"
)

func NewClient(c *http.Client) *api.Services {
	if c == nil {
		c = http.DefaultClient
	}
	httpClient := &httpClient{
		httpClient: c,
		BaseURL:    DefaultBaseURL,
		apiRouter:  router.NewAPIRouter(),
	}
	return &api.Services{
		Players: &playersService{httpClient},
	}
}

var ErrTODO = errors.New("TODO")
