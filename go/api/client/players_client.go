package client

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strconv"

	api "github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/api/router"
	"github.com/danmane/abalone/go/thirdparty/httputil2"
)

type playersClient struct {
	client *APIClient
}

func (s *playersClient) List() ([]api.Player, error) {
	path, err := s.client.apiRouter.Get(router.Players).URL()
	if err != nil {
		return nil, err
	}
	url := s.client.BaseURL + path.String()
	resp, err := http.Get(url)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != http.StatusOK {
		return nil, newHTTPError(resp)
	}
	var players []api.Player
	if err := json.NewDecoder(resp.Body).Decode(&players); err != nil {
		return nil, fmt.Errorf("error decoding json response: %s", err)
	}
	return players, nil
}

func (s *playersClient) Create(userID int64, p api.Player) (*api.Player, error) {
	p.AuthorId = userID
	var buf bytes.Buffer
	if err := json.NewEncoder(&buf).Encode(&p); err != nil {
		return nil, err
	}
	path, err := router.NewAPIRouter().Get(router.PlayersCreate).URL()
	if err != nil {
		return nil, err
	}
	resp, err := http.Post(s.client.BaseURL+path.String(), "application/json", &buf)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != http.StatusOK {
		return nil, newHTTPError(resp)
	}
	var player api.Player
	if err := json.NewDecoder(resp.Body).Decode(&player); err != nil {
		return nil, err
	}
	return &player, nil
}

func (s *playersClient) Upload(userID int64, p api.Player, executable io.Reader) (*api.Player, error) {
	params := map[string]string{
		"name":      p.Name,
		"version":   strconv.FormatInt(p.Version, 10),
		"author_id": strconv.FormatInt(userID, 10),
	}
	path, err := router.NewAPIRouter().Get(router.PlayersCreate).URL()
	if err != nil {
		return nil, err
	}
	url := s.client.BaseURL + path.String()

	req, err := httputil2.NewFileUploadRequest(url, params, "exe", "exe", executable)
	if err != nil {
		return nil, err
	}
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != http.StatusOK {
		return nil, httputil2.NewHTTPError(resp)
	}
	var player api.Player
	if err := json.NewDecoder(resp.Body).Decode(&player); err != nil {
		return nil, err
	}
	return &player, nil
}

func (s *playersClient) Delete(id int64) error {
	url, err := s.client.apiRouter.Get(router.PlayersDelete).URL("id", strconv.FormatInt(id, 10))
	if err != nil {
		return err
	}
	req, err := http.NewRequest("DELETE", s.client.BaseURL+url.String(), nil)
	if err != nil {
		return err
	}
	res, err := s.client.httpClient.Do(req)
	if err != nil {
		return err
	}
	defer res.Body.Close()
	if res.StatusCode != http.StatusOK {
		return newHTTPError(res)
	}
	return nil

}

var _ api.PlayersService = &playersClient{}
