package main

import (
	"bytes"
	"io"
	"log"
	"net/http"
)

func respondGame(cResponse chan<- io.Reader) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		log.Println("responding to /response")
		var buf bytes.Buffer
		io.Copy(&buf, r.Body)
		cResponse <- &buf
	}
}

func recieveGame(cGame chan<- io.Reader, cResponse <-chan io.Reader) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		log.Println("responding to /game")
		cGame <- r.Body
		body := <-cResponse
		log.Println("resolving /game")
		io.Copy(w, body)
	}
}

func respondPoll(cGame <-chan io.Reader) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		log.Println("responding to /poll")
		select {
		case g := <-cGame:
			// w.Header().Set("response-type", "application/json")
			log.Println("sending game to frontend")
			io.Copy(w, g)
		default:
			log.Println("nothing to report")
			w.WriteHeader(http.StatusNoContent)
		}
	}
}

func main() {
	cGame := make(chan io.Reader)
	cResponse := make(chan io.Reader)
	http.HandleFunc("/game", recieveGame(cGame, cResponse))
	http.HandleFunc("/poll", respondPoll(cGame))
	http.HandleFunc("/response", respondGame(cResponse))
	log.Fatal(http.ListenAndServe(":8999", nil))
}
