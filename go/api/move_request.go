package api

import (
	"time"

	"github.com/danmane/abalone/go/game"
)

const (
	MovePath         = "/move"
	PingPath         = "/ping"
	DefaultMoveLimit = 2 * time.Second
)

type MoveRequest struct {
	State      game.State `json:"state"`
	LimitMilli int64      `json:"limit_milli"`
}
