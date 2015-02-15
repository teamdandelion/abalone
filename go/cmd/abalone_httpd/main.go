package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"html/template"
	"io"
	"log"
	"net/http"
	"os"
	"path"
	"time"

	"github.com/cenkalti/backoff"
	"github.com/facebookgo/stackerr"
	docker "github.com/fsouza/go-dockerclient"
	"github.com/gorilla/mux"
)

// If |useTLS| is enabled, look for the cert files here.
var defaultCertPath = os.Getenv("DOCKER_CERT_PATH")

var (
	staticPath  = flag.String("static", "./static", "serve static files located in this directory")
	dockerHost  = flag.String("docker", "tcp://192.168.59.103:2376", "address of docker daemon host")
	tlsCertPath = flag.String("tls-cert-path", defaultCertPath, "path to docker host TLS certificate")
	host        = flag.String("host", ":8080", "address:port for HTTP listener")
	useTLS      = flag.Bool("tls", true, "use Docker TLS")
)

func main() {
	flag.Parse()
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	var client *docker.Client
	var err error
	if *useTLS {
		client, err = docker.NewTLSClient(
			*dockerHost,
			path.Join(*tlsCertPath, "cert.pem"),
			path.Join(*tlsCertPath, "key.pem"),
			path.Join(*tlsCertPath, "ca.pem"),
		)
		if err != nil {
			return stackerr.Wrap(err)
		}
	} else {
		client, err = docker.NewClient(*dockerHost)
		if err != nil {
			return err
		}
	}
	s := &AgentSupervisor{Client: client}
	log.Fatal(http.ListenAndServe(*host, Router(s, *staticPath)))
	return nil
}

// Router defines URL routes
func Router(s *AgentSupervisor, path string) *mux.Router {
	r := mux.NewRouter()
	WireAPIRoutes(r, s)
	WireHTMLRoutes(r, s) // after API routes

	// Finally, if none of the above routes match, delegate to the single-page
	// app's client-side router. Rewrite the path in order to load the
	// single-page app's root HTML entrypoint. The app will handle the route.
	r.NotFoundHandler = http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		r.URL.Path = "/"
		http.FileServer(http.Dir(path)).ServeHTTP(w, r)
	})
	return r
}

var templateHome = `
<html>
<body>
fxx >>= ai

	<form action=/api/v0/validate method=GET>
	<br>
	validate your AI... Enter the name of the Docker image:
		<input type=text name=image>
		</input>
	</form>
</body>
</html>
`

func WireHTMLRoutes(r *mux.Router, s *AgentSupervisor) {

	r.HandleFunc("/html", func(w http.ResponseWriter, r *http.Request) {
		tmpl, err := template.New("name").Parse(templateHome)
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		// Error checking elided
		if err := tmpl.Execute(w, nil); err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
	})
}

func WireAPIRoutes(r *mux.Router, s *AgentSupervisor) {

	apiV0 := r.PathPrefix("/api/v0").Subrouter()

	// TODO Build an agent from a GitHub repo
	// TODO Run Game between two running agents

	agents := apiV0.Path("/agents").Subrouter()
	agents.Methods("GET").HandlerFunc(ListAgentsHandler(s)) // list all available agents

	// pull an agent from Dockerhub
	dockerhub := apiV0.Path("/pull/dockerhub").Subrouter()
	dockerhub.Methods("GET").HandlerFunc(PullDockerHubAgentHandler(s)) // FIXME make this a POST

	// TODO List running agents with parameter
	apiV0.Path("/running").HandlerFunc(ListActiveAgentsHandler(s))

	// checks that the agent implements the right protocol
	apiV0.Path("/validate").HandlerFunc(ValidateAgentHandler(s))

	apiV0.Path("/image").HandlerFunc(ShowAgentInfoHandler(s))

	// If no API routes matched, but path had API prefix, return 404.
	apiV0.Path("/{rest:.*}").HandlerFunc(http.NotFound)
}

// PullDockerHubAgentHandler pulls the Docker image named |image| from
// DockerHub.
func PullDockerHubAgentHandler(s *AgentSupervisor) http.HandlerFunc {
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
func ListAgentsHandler(s *AgentSupervisor) http.HandlerFunc {
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
func ListActiveAgentsHandler(s *AgentSupervisor) http.HandlerFunc {
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
func ValidateAgentHandler(s *AgentSupervisor) http.HandlerFunc {
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
func ShowAgentInfoHandler(s *AgentSupervisor) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		image, err := s.Client.InspectImage("jbenet/go-ipfs")
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		json.NewEncoder(w).Encode(image.Config.ExposedPorts)
	}
}

// AgentSupervisor manages AI agents running in Docker containers
type AgentSupervisor struct {
	Client *docker.Client
}

func (s *AgentSupervisor) ValidateImage(image string) error {

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

	const kStopContainerTimeout = 5 // seconds
	if err := s.Client.StopContainer(container.ID, kStopContainerTimeout); err != nil {
		return err
	}
	return nil
}
