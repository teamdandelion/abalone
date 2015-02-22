package main

import (
	"net/http"
	"strconv"

	"github.com/gorilla/mux"
)

func getID(r *http.Request) (int64, error) {
	return strconv.ParseInt(mux.Vars(r)["id"], 10, 64)
}
