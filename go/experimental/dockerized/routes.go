package dockerized

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"time"

	"github.com/cenkalti/backoff"
	"github.com/facebookgo/stackerr"
	docker "github.com/fsouza/go-dockerclient"
)

type Config struct {
	DockerHost string // address of docker daemon host
	TLS        *TLSConfig
}

// paths to TLS config files
type TLSConfig struct {
	KeyPath           string
	CertPath          string
	CertAuthorityPath string
}

var DefaultConfig = Config{
	DockerHost: "tcp://127.0.0.1:2376",
}

func NewSupervisor(config Config) (*Supervisor, error) {
	var client *docker.Client
	var err error
	if config.TLS != nil {
		client, err = docker.NewTLSClient(
			config.DockerHost,
			config.TLS.CertPath,
			config.TLS.KeyPath,
			config.TLS.CertAuthorityPath,
		)
		if err != nil {
			return nil, stackerr.Wrap(err)
		}
	} else {
		client, err = docker.NewClient(config.DockerHost)
		if err != nil {
			return nil, err
		}
	}
	s := &Supervisor{Client: client}
	return s, nil
}

// PullDockerHubAgentHandler pulls the Docker image named |image| from
// DockerHub.
func PullDockerHubAgentHandler(s *Supervisor) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if r.FormValue("image") == "" {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		if err := s.Client.PullImage(docker.PullImageOptions{
			OutputStream: w,
			Registry:     "https://index.docker.io",
			Repository:   r.FormValue("image"),
			Tag:          "latest",
		}, docker.AuthConfiguration{}); err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			fmt.Fprintln(w, err)
		}
	}
}

// ListAgentsHandler lists AI agents.
func ListAgentsHandler(s *Supervisor) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		images, err := s.Client.ListImages(docker.ListImagesOptions{
			All: false,
		})
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			_, err := io.WriteString(w, err.Error())
			if err != nil {
				log.Println("error writing err: %s", err)
			}
			return
		}
		for _, img := range images {
			_, err := fmt.Fprintln(w, fmt.Sprintln(append([]string{img.ID, "\t"}, img.RepoTags...)))
			if err != nil {
				w.WriteHeader(http.StatusInternalServerError)
				_, err := io.WriteString(w, err.Error())
				if err != nil {
					log.Println("error writing err: %s", err)
				}
			}
		}
	}
}

// ListActiveAgentsHandler lists agents that are currently running.
func ListActiveAgentsHandler(s *Supervisor) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		containers, err := s.Client.ListContainers(docker.ListContainersOptions{})
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		for _, ps := range containers {
			fmt.Fprintln(w, fmt.Sprintf("%+v\n", ps))
		}
	}
}

// ValidateAgentHandler ensures that the Agent image responds to the PING
// command.
func ValidateAgentHandler(s *Supervisor) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		defer log.Println("returned")

		// client gives the |image| as a URL parameter
		if r.FormValue("image") == "" {
			w.WriteHeader(http.StatusBadRequest)
			fmt.Fprintln(w, "`image` parameter is required")
			return
		}
		image := r.FormValue("image")

		if err := s.ValidateImage(image); err != nil {
			fmt.Fprintf(w, "image %s is not valid. error: %s", image, err)
			return
		}
		fmt.Fprintf(w, "image %s is valid", image)
	}
}

type AgentInfo struct {
	Owner  string
	Taunts []string
}

// ShowImageHandler shows information about an AI Agent
func ShowAgentInfoHandler(s *Supervisor) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		image, err := s.Client.InspectImage("jbenet/go-ipfs")
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		json.NewEncoder(w).Encode(image.Config.ExposedPorts)
	}
}

// UploadImageHandler ensures that the Agent image responds to the PING
// command.
func UploadImageHandler(s *Supervisor) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		rs := struct {
			Image  string
			Source string
		}{}
		if err := json.NewDecoder(r.Body).Decode(&rs); err != nil {
			w.WriteHeader(http.StatusBadRequest)
			fmt.Fprintf(w, "error decoding request: %s", err)
			return
		}
		switch rs.Source {
		case "dockerhub":
			if err := s.ValidateImage(rs.Image); err != nil {
				w.WriteHeader(http.StatusBadRequest)
				fmt.Fprintf(w, "image %s is not valid. error: %s", rs.Image, err)
				return
			}
		case "github":
			w.WriteHeader(http.StatusNotImplemented)
			fmt.Fprintln(w, "Sorry. GitHub repo support has not been implemented yet.")
		default:
			w.WriteHeader(http.StatusBadRequest)
			fmt.Fprintf(w, "Unrecognized image source: %s", rs.Source)
		}
	}
}

// Supervisor manages AI agents running in Docker containers
type Supervisor struct {
	Client *docker.Client
}

func (s *Supervisor) ValidateImage(image string) error {

	// run the container in a two-phase process. First, create the
	// container.
	container, err := s.Client.CreateContainer(docker.CreateContainerOptions{
		Config: &docker.Config{
			Image: image, // the only required argument
		},
	})
	if err != nil {
		return fmt.Errorf("error creating container:", err.Error())
	}

	hc := &docker.HostConfig{
		PublishAllPorts: true,
	}

	// run the created container
	if err := s.Client.StartContainer(container.ID, hc); err != nil {
		return fmt.Errorf("error starting container: %s", err.Error())
	}

	// ensure the proper port is exposed
	info, err := s.Client.InspectContainer(container.ID)
	if err != nil {
		return fmt.Errorf("error inspecting container: %s", err.Error())
	}

	mappings, ok := info.NetworkSettings.Ports[docker.Port("3423/tcp")]
	if !ok {
		return fmt.Errorf(
			"container must expose port 3423/tcp. Found: %+v",
			info.NetworkSettings.Ports)
	}
	if len(mappings) != 1 {
		return fmt.Errorf(
			"error. expected one port mapping. found: %+v",
			info.NetworkSettings.Ports)
	}
	ip, port := mappings[0].HostIP, mappings[0].HostPort

	backoffConfig := backoff.NewExponentialBackOff()
	backoffConfig.InitialInterval = time.Second
	backoffConfig.MaxInterval = 10
	backoffConfig.MaxElapsedTime = 10 * time.Second
	err = backoff.Retry(func() error {
		resp, err := http.Get(fmt.Sprintf("http://%s:%s/ping", ip, port))
		if err != nil {
			log.Println("error pinging agent. found:", err)
			// TODO handle err
			return err
		}
		defer resp.Body.Close()
		var agentInfo AgentInfo
		if err := json.NewDecoder(resp.Body).Decode(&agentInfo); err != nil {
			return err
		}
		if agentInfo.Owner == "btc" {
			log.Println("yay!")
		}
		return nil
	}, backoffConfig)

	// TODO check error in case ping didn't work

	const kStopContainerTimeout = 5 // seconds
	if err := s.Client.StopContainer(container.ID, kStopContainerTimeout); err != nil {
		return err
	}
	return nil
}
