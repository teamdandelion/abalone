package operator

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"time"

	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/game"
)

// PlayerProcessInstance is not thread-safe
type PlayerProcessInstance struct {
	remote    RemotePlayerInstance
	closeFunc func() error
}

func (i *PlayerProcessInstance) Player() api.Player {
	return i.remote.Player()
}

func (i *PlayerProcessInstance) Play(s *game.State, limit time.Duration) (*game.State, error) {
	return i.remote.Play(s, limit)
}

func (i *PlayerProcessInstance) Close() error {
	if i.closeFunc == nil {
		return errors.New("player process is already closed")
	}
	err := i.closeFunc()
	if err != nil {
		return err
	}
	i.closeFunc = nil
	return nil
}

func Validate(pathToExecutable string, scheduler *PortScheduler) error {
	var player api.Player
	ppi, err := NewPlayerProcessInstance(player, pathToExecutable, scheduler)
	if err != nil {
		fmt.Printf("ai at path %v\n failed validating", pathToExecutable)
		return err
	}
	// TODO(btc): note that this functon won't close the player when an error
	// is returned (above). need to ensure port is released and process is killed.
	return ppi.Close()
}

func NewPlayerProcessInstance(player api.Player, executable string, scheduler *PortScheduler) (*PlayerProcessInstance, error) {
	port, err := scheduler.GetPort()
	if err != nil {
		return nil, err
	}
	host := fmt.Sprintf("localhost:%v", port)
	aiCmd := exec.Command(executable, fmt.Sprintf("-port=%v", port))
	aiCmd.Stdout = os.Stdout
	aiCmd.Stderr = os.Stderr

	if err := aiCmd.Start(); err != nil {
		return nil, err
	}

	rpi := RemotePlayerInstance{APIPlayer: player, Host: host}
	if err := rpi.Ping(); err != nil {
		fmt.Println("ping failure")
		return nil, err
	}

	return &PlayerProcessInstance{
		remote: rpi,

		closeFunc: func() error {
			err := aiCmd.Process.Kill()
			if err != nil {
				return err
			}
			scheduler.ReleasePort(port)
			return nil
		},
	}, nil
}
