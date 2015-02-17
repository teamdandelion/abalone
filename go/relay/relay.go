package main

import (
	"bytes"
	"io"
	"log"
	"net/http"
)

func responseFromFrontend(cGame <-chan io.Reader, cFrontendResponse chan<- io.Reader) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		log.Println("responding to /frontend")
		var buf bytes.Buffer
		if r.ContentLength != 0 {
			io.Copy(&buf, r.Body)
			cFrontendResponse <- &buf
		} else {
			log.Println("received empty post to /frontend, not pushing to channel")
		}

		resultantGame := <-cGame
		log.Println("sending game back to /frontend")
		w.Header().Set("Access-Control-Allow-Origin", "*")
		io.Copy(w, resultantGame)
	}
}

func gameFromOperator(cGame chan<- io.Reader, cFrontendResponse <-chan io.Reader) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		log.Println("responding to /game")
		cGame <- r.Body
		body := <-cFrontendResponse
		log.Println("resolving /game")
		io.Copy(w, body)
	}
}

func main() {
	cGame := make(chan io.Reader)
	cFrontendResponse := make(chan io.Reader)
	http.HandleFunc("/game", gameFromOperator(cGame, cFrontendResponse))
	http.HandleFunc("/frontend", responseFromFrontend(cGame, cFrontendResponse))
	log.Fatal(http.ListenAndServe(":1337", nil))
}

/* Sequence:

FE POST ""
OP POST Game1
FE RECV Game1
"" is thrown away.
FE POST Game1
OP RECV Game2
OP POST Game3
FE RECV Game3


*/
