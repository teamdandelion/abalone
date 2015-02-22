package client

import (
	"bytes"
	"encoding/json"
	"net/http"

	api "github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/api/router"
)

type matchesClient struct {
	client *APIClient
}

func (s *matchesClient) Run(playerID1, playerID2 int64) (*api.Match, error) {
	match := api.Match{
		PID1: playerID1,
		PID2: playerID2,
	}
	var buf bytes.Buffer
	if err := json.NewEncoder(&buf).Encode(&match); err != nil {
		return nil, err
	}
	path, err := s.client.apiRouter.Get(router.MatchesRun).URL()
	if err != nil {
		return nil, err
	}
	url := s.client.BaseURL + path.String()
	resp, err := http.Post(url, "application/json", &buf)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != http.StatusOK {
		return nil, newHTTPError(resp)
	}

	var created api.Match
	if err := json.NewDecoder(resp.Body).Decode(&created); err != nil {
		return nil, err
	}
	return &created, nil
}

var _ api.MatchesService = &matchesClient{}
