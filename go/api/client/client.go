package client

import (
	"bytes"
	"fmt"
	"io"
	"net/http"
	"net/url"

	api "github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/api/router"
	"github.com/gorilla/mux"
)

type APIClient struct {
	BaseURL string

	httpClient *http.Client
	apiRouter  *mux.Router
}

const (
	DefaultBaseURL = "http://localhost:8080" // TODO(btc): http://api.foo.ai
)

func NewClient(opts ...Option) *api.Services {
	client := &APIClient{
		httpClient: http.DefaultClient,
		BaseURL:    DefaultBaseURL,
		apiRouter:  router.NewAPIRouter(),
	}
	for _, o := range opts {
		o(client)
	}
	return &api.Services{
		Players: &playersClient{client},
		Users:   &usersClient{client},
	}
}

func (c *APIClient) ParseBaseURL(s string) error {
	url, err := url.Parse(s)
	if err != nil {
		return err
	}
	c.BaseURL = url.String()
	return nil
}

type Option func(*APIClient)

func BaseURL(s string) Option {
	return func(c *APIClient) {
		c.BaseURL = s
	}
}

func HTTPClient(httpClient *http.Client) Option {
	return func(c *APIClient) {
		c.httpClient = httpClient
	}
}

func newHTTPError(resp *http.Response) error {
	return fmt.Errorf("error: %s; %s", resp.Status, rtos(resp.Body))
}

func rtos(r io.Reader) string {
	var buf bytes.Buffer
	io.Copy(&buf, r)
	return buf.String()
}
