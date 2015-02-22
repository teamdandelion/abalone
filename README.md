This is a AI programming competition for the classic board game [Abalone](http://en.wikipedia.org/wiki/Abalone_%28board_game%29). 
====


Installation:
---
go get github.com/danmane/abalone/...
cd $GOPATH/github.com/danmane/abalone/go
go get ./...
go build ./...
cd $GOPATH/github.com/danmane/abalone/frontend
npm install
bower install


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
