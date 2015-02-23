package operator

import (
	"fmt"
	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/game"
	"os"
	"os/exec"
	"time"
)

type PlayerProcessInstance struct {
	APIPlayer api.Player
	Path      string
	remote    RemotePlayerInstance
	aiCmd     *exec.Cmd
	port      int
	scheduler PortScheduler
}

func (i *PlayerProcessInstance) Player() api.Player {
	return i.APIPlayer
}

func (i *PlayerProcessInstance) Play(s *game.State, limit time.Duration) (*game.State, error) {
	return i.remote.Play(s, limit)
}

func (i *PlayerProcessInstance) Close() error {
	err := i.aiCmd.Process.Kill()
	if err != nil {
		return err
	}
	i.scheduler.ReleasePort(i.port)
	return nil
}

func Validate(path string, scheduler PortScheduler) error {
	var player api.Player
	player.Path = path
	ppi, err := NewPlayerProcessInstance(player, scheduler)
	if err != nil {
		fmt.Printf("ai at path %v\n failed validating", path)
		return err
	}
	defer ppi.Close()
	return nil
}

func NewPlayerProcessInstance(player api.Player, scheduler PortScheduler) (*PlayerProcessInstance, error) {
	port, err := scheduler.GetPort()
	if err != nil {
		return nil, err
	}
	host := fmt.Sprintf("localhost:%v", port)
	aiCmd := exec.Command(player.Path, fmt.Sprintf("-port=%v", port))
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
		APIPlayer: player,
		Path:      player.Path,
		port:      port,
		remote:    rpi,
		aiCmd:     aiCmd,
		scheduler: scheduler,
	}, nil
}
