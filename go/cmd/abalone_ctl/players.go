package main

import (
	"fmt"
	"io"
	"log"
	"strconv"

	"github.com/codegangsta/cli"
	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/api/client"
	"github.com/olekukonko/tablewriter"
)

const (
	argName   = "name"
	argVer    = "version"
	argHost   = "host"
	argAuthor = "author"
)

var PlayersCmd = cli.Command{
	Name:      "players",
	ShortName: "p",
	Usage:     "manage ai players",
	Action: func(c *cli.Context) {
		if err := ListPlayersHandler(c); err != nil {
			log.Fatal(err)
		}
	},
	Subcommands: []cli.Command{
		{
			Name:      "create",
			ShortName: "c",
			Usage:     "create a player",
			Flags: []cli.Flag{
				cli.StringFlag{
					Name:  "name, n",
					Usage: "name used to identify player",
				},
				cli.IntFlag{
					Name:  "version, v",
					Value: 1,
				},
				cli.StringFlag{
					Name:  "host",
					Usage: "ip:port where player can be reached (e.g. localhost:3423 or api.player.ai:80)",
				},
				cli.IntFlag{
					Name:  "author, a",
					Usage: "id of player's author (user)",
				},
			},
			Action: func(c *cli.Context) {
				if err := CreatePlayersHandler(c); err != nil {
					log.Fatal(err)
				}
			},
		},
		{
			Name:      "list",
			ShortName: "l",
			Usage:     "list players",
			Action: func(c *cli.Context) {
				if err := ListPlayersHandler(c); err != nil {
					log.Fatal(err)
				}
			},
		},
		{
			Name:      "delete",
			ShortName: "d",
			Usage:     "delete player by id",
			Action: func(c *cli.Context) {
				if err := DeletePlayersHandler(c); err != nil {
					log.Fatal(err)
				}
			},
		},
	},
}

func CreatePlayersHandler(c *cli.Context) error {

	if !c.IsSet(argName) {
		return ErrArgRequired(argName)
	}
	n := c.String(argName)

	if !c.IsSet(argVer) {
		return ErrArgRequired(argVer)
	}
	v := c.Int(argVer)

	if !c.IsSet(argHost) {
		return ErrArgRequired(argHost)
	}
	h := c.String(argHost)

	if !c.IsSet(argAuthor) {
		return ErrArgRequired(argAuthor)
	}
	a := c.Int(argAuthor)

	req := api.Player{
		Name:     n,
		Version:  int64(v),
		Host:     h,
		AuthorId: int64(a),
	}

	client := client.NewClient(client.BaseURL(c.GlobalString("httpd")))
	player, err := client.Players.Create(int64(a), req)
	if err != nil {
		return err
	}
	return printPlayers(c.App.Writer, []api.Player{*player})
}

func ListPlayersHandler(c *cli.Context) error {
	client := client.NewClient(client.BaseURL(c.GlobalString("httpd")))
	players, err := client.Players.List()
	if err != nil {
		return err
	}
	return printPlayers(c.App.Writer, players)
}

func DeletePlayersHandler(c *cli.Context) error {
	client := client.NewClient(client.BaseURL(c.GlobalString("httpd")))

	id, err := strconv.ParseInt(c.Args().First(), 10, 64)
	if err != nil {
		return fmt.Errorf("error parsing player id: %s", err)
	}
	if err := client.Players.Delete(id); err != nil {
		return err
	}
	return nil
}

func printPlayers(w io.Writer, players []api.Player) error {
	table := tablewriter.NewWriter(w)
	table.SetHeader([]string{"ID", "Name", "Version", "Author", "Created", "Updated"})
	for _, p := range players {
		row := []string{
			strconv.FormatInt(p.ID, 10),
			p.Name,
			strconv.FormatInt(p.Version, 10),
			strconv.FormatInt(p.AuthorId, 10),
			p.CreatedAt.Format(TimeSimpleFmt),
			p.UpdatedAt.Format(TimeSimpleFmt),
		}
		table.Append(row)
	}
	table.Render() // Send output
	return nil
}
