package main

import (
	"errors"
	"fmt"
	"io"
	"log"
	"strconv"

	"github.com/codegangsta/cli"
	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/api/client"
	"github.com/olekukonko/tablewriter"
)

var UsersCmd = cli.Command{
	Name:      "users",
	ShortName: "u",
	Usage:     "manage abalone users",
	Action: func(c *cli.Context) {
		if err := ListUsersHandler(c); err != nil {
			log.Fatal(err)
		}
	},
	Subcommands: []cli.Command{
		{
			Name:      "create",
			ShortName: "c",
			Usage:     "create a user",
			Flags: []cli.Flag{
				cli.StringFlag{
					Name:  "name, n",
					Value: "",
				},
				cli.StringFlag{
					Name:  "email, e",
					Value: "",
				},
			},
			Action: func(c *cli.Context) {
				if err := CreateUsersHandler(c); err != nil {
					log.Fatal(err)
				}
			},
		},
		{
			Name:      "list",
			ShortName: "l",
			Usage:     "list users",
			Action: func(c *cli.Context) {
				if err := ListUsersHandler(c); err != nil {
					log.Fatal(err)
				}
			},
		},
		{
			Name:      "delete",
			ShortName: "d",
			Usage:     "delete user by id",
			Action: func(c *cli.Context) {
				if err := DeleteUsersHandler(c); err != nil {
					log.Fatal(err)
				}
			},
		},
	},
}

func CreateUsersHandler(c *cli.Context) error {
	if !c.IsSet("name") {
		return errors.New("name is required")
	}
	n := c.String("name")

	if !c.IsSet("email") {
		return errors.New("email is required")
	}
	e := c.String("email")

	req := api.User{
		Name:  n,
		Email: e,
	}
	client := client.NewClient(client.BaseURL(c.GlobalString("httpd")))
	user, err := client.Users.Create(req)
	if err != nil {
		return err
	}
	return printUsers(c.App.Writer, []api.User{*user})
}

func ListUsersHandler(c *cli.Context) error {
	client := client.NewClient(client.BaseURL(c.GlobalString("httpd")))
	users, err := client.Users.List()
	if err != nil {
		return err
	}
	return printUsers(c.App.Writer, users)
}

func DeleteUsersHandler(c *cli.Context) error {
	client := client.NewClient(client.BaseURL(c.GlobalString("httpd")))
	id, err := strconv.ParseInt(c.Args().First(), 10, 64)
	if err != nil {
		return fmt.Errorf("error parsing user id: %s", err)
	}
	if err := client.Users.Delete(id); err != nil {
		return err
	}
	return nil
}

func printUsers(w io.Writer, users []api.User) error {
	table := tablewriter.NewWriter(w)
	table.SetHeader([]string{"ID", "Name", "Email", "Created", "Updated"})
	for _, u := range users {
		row := []string{
			strconv.FormatInt(u.ID, 10),
			u.Name,
			u.Email,
			u.CreatedAt.Format(TimeSimpleFmt),
			u.UpdatedAt.Format(TimeSimpleFmt),
		}
		table.Append(row)
	}
	table.Render() // Send output
	return nil
}
