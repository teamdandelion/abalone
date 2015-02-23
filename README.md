This is a AI programming competition for the classic board game [Abalone](http://en.wikipedia.org/wiki/Abalone_%28board_game%29). 
====


Installation:
---
```
go get github.com/danmane/abalone/...
cd $GOPATH/github.com/danmane/abalone/go
go get ./...
go build ./...
cd $GOPATH/github.com/danmane/abalone/frontend
npm install
bower install
```

Getting Started:
---
We've made it  easy to develop an AI, provided you're comfortable using Go. Just take the template in `go/cmd/abalone_example_ai/main.go` and fill the skeleton `play` function with your own AI logic. An implementation of the game is already provided in `go/game` so you don't need to write the logic yourself.

Writing an AI in a different language:
---

Behind the scenes, the `quickstart` package will take care of implementing networking and communication for you. If you want to write an AI in a different language, it will need to implement the following spec:
- It is an executable that will take a "-port" argument at the command line, which will tell it which port to listen on.
- It will bind to that port. If it receives a GET request to "/ping", it will respond with http.StatusOK (200).
- If it recieves a POST request to "/move", it will parse the incoming JSON as a `MoveRequest` (move_request.go) object containing the current Abalone game state and the number of milliseconds it has to respond. It will respond within that number of milliseconds with a valid game state one move advanced from the one it recieved. 


### To play locally against yourself or another human
```
cd frontend
grunt
localhost:9999/local.html
```

### To play against an AI with yourself as black
```
cd frontend
grunt&
./cmd/abalone_example_ai/abalone_example_ai& #defaults to port 3423, which signifies playing as white
./cmd/abalone_relayd/abalone_relayd& #defaults to port 3423, which signifies playing as black
./cmd/abalone_gameoperator/abalone_gameoperator
navigate to localhost:9999/remote.html
```

### To play against an AI with yourself as white
```
cd frontend
grunt&
cd ../go 
./cmd/abalone_example_ai/abalone_example_ai -port="3424"&
./cmd/abalone_relayd/abalone_relayd -port="3423&
./cmd/abalone_gameoperator/abalone_gameoperator
navigate to localhost:9999/remote.html
```



### To play two AIs agaisnt each other
```
./cmd/abalone_example_ai/abalone_example_ai -port="3423"&
./cmd/abalone_example_ai/abalone_example_ai -port="3424"&
./cmd/abalone_gameoperator/abalone_gameoperator
```
