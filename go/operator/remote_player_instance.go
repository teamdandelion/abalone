package operator

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"time"

	"github.com/cenkalti/backoff"
	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/game"
)

type RemotePlayerInstance struct {
	APIPlayer api.Player
	Host      string
}

func (i *RemotePlayerInstance) Player() api.Player {
	return i.APIPlayer
}

func (i *RemotePlayerInstance) Play(s *game.State, limit time.Duration) (*game.State, error) {
	return gameFromAI(i.Host, s)
}

func (i *RemotePlayerInstance) Ping() error {
	resp, err := http.Get("http://" + i.Host + api.PingPath)
	if err != nil {
		return err
	}
	if resp.StatusCode != 200 {
		return fmt.Errorf("expected /ping to return 200 but got %v", resp.StatusCode)
	}
	return nil
}

func gameFromAI(host string, state *game.State) (*game.State, error) {
	mr := api.MoveRequest{
		State:      *state,
		LimitMilli: toMillisecondCount(api.DefaultMoveLimit),
	}
	var buf bytes.Buffer
	if err := json.NewEncoder(&buf).Encode(&mr); err != nil {
		return nil, err
	}

	var rawResponse bytes.Buffer // will contain response if HTTP request is successful
	f := func() error {
		resp, err := http.Post("http://"+host+api.MovePath, "application/json", bytes.NewBuffer(buf.Bytes()))
		if err != nil {
			return err
		}
		defer resp.Body.Close()
		io.Copy(&rawResponse, resp.Body)
		return nil
	}

	if err := withRetries(f); err != nil {
		return nil, err
	}

	responseGame := &game.State{}
	if err := json.NewDecoder(&rawResponse).Decode(responseGame); err != nil {
		return nil, err
	}
	if !state.ValidFuture(responseGame) {
		return nil, fmt.Errorf("game parsed correctly, but isn't a valid future")
	}
	return responseGame, nil
}

func withRetries(f func() error) error {
	backoffConfig := backoff.NewExponentialBackOff()
	backoffConfig.InitialInterval = time.Second
	backoffConfig.MaxInterval = 10 * time.Second
	backoffConfig.MaxElapsedTime = 60 * time.Second

	notifyFunc := func(err error, dur time.Duration) {
		log.Printf("waiting %v, failed to get move from player: %s", dur, err)
	}
	err := backoff.RetryNotify(f, backoffConfig, notifyFunc)
	return err
}

func toMillisecondCount(d time.Duration) int64 {
	return int64(d / 1e6)
}
