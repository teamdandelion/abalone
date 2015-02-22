package client

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"
	"strconv"

	api "github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/api/router"
)

type usersClient struct {
	client *APIClient
}

func (s *usersClient) Create(user api.User) (*api.User, error) {
	var buf bytes.Buffer
	if err := json.NewEncoder(&buf).Encode(&user); err != nil {
		return nil, err
	}
	path, err := s.client.apiRouter.Get(router.UsersCreate).URL()
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

	var created api.User
	if err := json.NewDecoder(resp.Body).Decode(&created); err != nil {
		return nil, err
	}
	return &created, nil
}

// TODO(btc) add pagination
func (s *usersClient) List() ([]api.User, error) {
	path, err := s.client.apiRouter.Get(router.Users).URL()
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
	var users []api.User
	if err := json.NewDecoder(resp.Body).Decode(&users); err != nil {
		return nil, fmt.Errorf("error decoding json response: %s", err)
	}
	return users, nil
}

func (s *usersClient) Delete(id int64) error {
	url, err := s.client.apiRouter.Get(router.UsersDelete).URL("id", strconv.FormatInt(id, 10))
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

var _ api.UsersService = &usersClient{}
