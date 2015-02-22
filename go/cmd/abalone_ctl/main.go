package main

import (
	"fmt"
	"log"
	"os"

	"github.com/codegangsta/cli"
)

const (
	DefaultHTTPDHost = "localhost:8080" // TODO extract
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

func ErrArgRequired(arg string) error {
	return fmt.Errorf("%s is required", arg)
}

func APIURL(c *cli.Context, path string) string {
	return fmt.Sprintf("http://%s%s", c.GlobalString("httpd"), path)
}
