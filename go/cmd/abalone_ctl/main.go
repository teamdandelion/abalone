package main

import (
	"log"
	"os"

	"github.com/codegangsta/cli"
)

const (
	DefaultHTTPDHost = "localhost:8080" // TODO extract
)

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

// TODO execute game on http daemon
func run() error {
	app := cli.NewApp()
	app.Name = "abalone_ctl"
	app.Usage = "abalone management util"
	app.Flags = []cli.Flag{
		cli.StringFlag{
			Name:  "httpd",
			Value: DefaultHTTPDHost,
			Usage: "abalone_httpd listening addr:port",
		},
	}
	app.Commands = []cli.Command{
		UsersCmd,
	}
	return app.Run(os.Args)
}
