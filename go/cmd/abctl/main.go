package main

import (
	"fmt"
	"log"
	"os"

	"github.com/codegangsta/cli"
	"github.com/danmane/abalone/go/api/client"
)

const (
	TimeSimpleFmt = "Mon Jan 2 15:04:05"
)

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

// TODO execute game on http daemon
func run() error {
	app := cli.NewApp()
	app.Author = "fxx"
	app.Email = ""
	app.Name = "abalone_ctl"
	app.Usage = "abalone management util"
	app.Flags = []cli.Flag{
		cli.StringFlag{
			Name:  "httpd",
			Value: client.DefaultBaseURL,
			Usage: "abalone_httpd listening addr:port",
			EnvVar:   "ABCTL_HTTPD",
		},
	}
	app.Commands = []cli.Command{
		MatchesCmd,
		PlayersCmd,
		UsersCmd,
	}
	return app.Run(os.Args)
}

func ErrArgRequired(arg string) error {
	return fmt.Errorf("%s is required", arg)
}
