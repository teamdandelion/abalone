package main

import (
	"bytes"
	"flag"
	"github.com/gorilla/mux"
	"io"
	"log"
	"net/http"
)

var (
	port = flag.String("port", "3424", "port the operator pings at")
)

func handleClientPost(clientPost chan<- []byte) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		log.Println("responding to /frontend")
		var buf bytes.Buffer
		io.Copy(&buf, r.Body)
		clientPost <- buf.Bytes()
	}
}

func handleClientGet(clientGet <-chan []byte) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		log.Println("handleClientGet: waiting for value from channel")
		w.Header().Set("Access-Control-Allow-Origin", "*")
		io.Copy(w, bytes.NewBuffer(<-clientGet))
		log.Println("handleClientGet: got resp from channel, resolving")
	}
}

func handleOperatorPost(operatorPost chan<- []byte, gameToOperator <-chan []byte) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		log.Println("responding to /move")
		var buf bytes.Buffer
		io.Copy(&buf, r.Body)
		operatorPost <- buf.Bytes()
		cp := <-gameToOperator
		log.Println("resolving /move")
		io.Copy(w, bytes.NewBuffer(cp))
	}
}

func main() {
	flag.Parse()
	r := mux.NewRouter()
	operatorPost := make(chan []byte)
	clientPost := make(chan []byte)
	clientGet := make(chan []byte)
	gameToOperator := make(chan []byte)
	r.Path("/frontend").Methods("GET").HandlerFunc(handleClientGet(clientGet))
	r.Path("/frontend").Methods("POST").HandlerFunc(handleClientPost(clientPost))
	r.Path("/move").Methods("POST").HandlerFunc(handleOperatorPost(operatorPost, gameToOperator))
	go func() {
		var previousGame []byte
		for {
			sendGameToClient := clientGet
			if previousGame == nil {
				sendGameToClient = nil
			}
			select {
			case x := <-clientPost:
				gameToOperator <- x
				previousGame = nil

			case x := <-operatorPost:
				previousGame = x

			case sendGameToClient <- previousGame:
			}
		}
	}()

	go func() {
		http.ListenAndServe(":"+*port, r)
	}()
	http.ListenAndServe(":1337", r)
}
