package main

import (
	"flag"
	"log"
	"net/http"

	"github.com/briantigerchow/gamecenter"
)

var mem = flag.Bool("mem", false, "run with an in-memory backed")
var db = flag.String("db", ":5432", "set the db host addr:port")
var host = flag.String("host", ":8080", "set the http host addr:port")

func main() {
	flag.Parse()
	log.Printf("listening at %s", *host)

	var b gamecenter.GamesBackend
	if *mem {
		log.Println("running with an in-memory backend...")
		b = gamecenter.NewMemBackend()
	} else {
		b = &gamecenter.PostgresBackend{*db}
	}
	log.Fatal(http.ListenAndServe(*host, gamecenter.Router(b)))
}
