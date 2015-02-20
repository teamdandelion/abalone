package main

import (
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"

	"github.com/codegangsta/cli"
	"github.com/danmane/abalone/go/router"
)

const (
	DefaultHTTPDHost = "localhost:8080" // TODO extract
)

var (
	httpdHost = flag.String("httpd", DefaultHTTPDHost, "abalone_httpd listening addr:port")
)

func main() {
	flag.Parse()
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

// TODO execute game on http daemon
func run() error {
	app := cli.NewApp()
	app.Name = "abalone_ctl"
	app.Usage = "abalone management util"
	app.Commands = []cli.Command{
		RunCmd,
	}
	return app.Run(os.Args)
}

var RunCmd = cli.Command{
	Name:   "run",
	Usage:  "runs a game on abalone_httpd",
	Action: RunGameAction,
}

func RunGameAction(c *cli.Context) {
	if err := doRunGame(c); err != nil {
		log.Fatal(err)
	}
}

func doRunGame(c *cli.Context) error {
	req := struct {
		BlackPort string
		WhitePort string
	}{
		BlackPort: "3423",
		WhitePort: "3424",
	}

	r := router.NewAPIRouter()
	path, err := r.Get(router.GamesRun).URL()
	if err != nil {
		return err
	}
	url := fmt.Sprintf("http://%s%s", *httpdHost, path.String())

	var buf bytes.Buffer
	if err := json.NewEncoder(&buf).Encode(&req); err != nil {
		return err
	}
	resp, err := http.Post(url, "application/json", &buf)
	if err != nil {
		return err
	}
	if resp.StatusCode != http.StatusOK {
		log.Println(resp.Status)
	}
	return nil
}
