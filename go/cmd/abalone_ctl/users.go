package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"log"
	"net/http"
	"strconv"

	"github.com/codegangsta/cli"
	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/router"
	"github.com/olekukonko/tablewriter"
)

var UsersCmd = cli.Command{
	Name:      "users",
	ShortName: "u",
	Usage:     "manage abalone users",
	Subcommands: []cli.Command{
		{
			Name:      "create",
			ShortName: "c",
			Usage:     "creates a user",
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
			Usage:     "lists users",
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
	r := router.NewAPIRouter()
	path, err := r.Get(router.UsersCreate).URL()
	if err != nil {
		return err
	}

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
	var buf bytes.Buffer
	if err := json.NewEncoder(&buf).Encode(&req); err != nil {
		return err
	}
	url := fmt.Sprintf("http://%s%s", c.GlobalString("httpd"), path.String())
	resp, err := http.Post(url, "application/json", &buf)
	if err != nil {
		return err
	}
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("error: %s", resp.Status)
	}
	return nil
}

func ListUsersHandler(c *cli.Context) error {
	r := router.NewAPIRouter()
	path, err := r.Get(router.Users).URL()
	if err != nil {
		return err
	}
	url := fmt.Sprintf("http://%s%s", c.GlobalString("httpd"), path.String())
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("error: %s", resp.Status)
	}
	var users []api.User
	if err := json.NewDecoder(resp.Body).Decode(&users); err != nil {
		return fmt.Errorf("error decoding json response: %s", err)
	}

	table := tablewriter.NewWriter(c.App.Writer)
	table.SetHeader([]string{"ID", "Name", "Email", "Created", "Updated"})
	for _, u := range users {
		row := []string{
			strconv.FormatInt(u.ID, 10),
			u.Name,
			u.Email,
			u.CreatedAt.Format("Mon Jan 2 15:04:05"),
			u.UpdatedAt.Format("Mon Jan 2 15:04:05"),
		}
		table.Append(row)
	}
	table.Render() // Send output
	return nil
}

func DeleteUsersHandler(c *cli.Context) error {
	r := router.NewAPIRouter()
	path, err := r.Get(router.UsersDelete).URL("id", c.Args().First())
	if err != nil {
		return fmt.Errorf("error parsing user id: %s", err)
	}
	url := fmt.Sprintf("http://%s%s", c.GlobalString("httpd"), path.String())
	req, err := http.NewRequest("DELETE", url, nil)
	if err != nil {
		return err
	}

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return err
	}
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("error: %s", resp.Status)
	}
	return nil
}
