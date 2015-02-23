package main

import (
	"io"
	"log"
	"strconv"

	"github.com/codegangsta/cli"
	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/api/client"
	"github.com/olekukonko/tablewriter"
)

var MatchesCmd = cli.Command{
	Name:      "matches",
	ShortName: "m",
	Usage:     "manage matches between players",
	// TODO(btc) ListMatchesHandler
	Subcommands: []cli.Command{
		{
			Name:      "run",
			ShortName: "r",
			Usage:     "run a match between two AI players (provide player_id for each player)",
			Action: func(c *cli.Context) {
				if err := RunMatchesHandler(c); err != nil {
					log.Fatal(err)
				}
			},
		},
	},
}

func RunMatchesHandler(c *cli.Context) error {
	ID1, err := strconv.ParseInt(c.Args().Get(0), 10, 64)
	if err != nil {
		return err
	}

	ID2, err := strconv.ParseInt(c.Args().Get(1), 10, 64)
	if err != nil {
		return err
	}
	log.Println(ID1, ID2)

	client := client.NewClient(client.BaseURL(c.GlobalString("httpd")))
	match, err := client.Matches.Run(ID1, ID2)
	if err != nil {
		return err
	}
	return printMatches(c.App.Writer, []api.Match{*match})
}

func printMatches(w io.Writer, matches []api.Match) error {
	table := tablewriter.NewWriter(w)
	table.SetHeader([]string{"ID", "Players", "Created", "Updated"})
	for _, m := range matches {
		row := []string{
			strconv.FormatInt(m.ID, 10),
			strconv.FormatInt(m.PID1, 10) + " " + strconv.FormatInt(m.PID2, 10),
			m.CreatedAt.Format(TimeSimpleFmt),
			m.UpdatedAt.Format(TimeSimpleFmt),
		}
		table.Append(row)
	}
	table.Render() // Send output
	return nil
}
