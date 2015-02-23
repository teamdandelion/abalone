all:
	# nop

vendor: godep
	go get ./...
	godep save ./...

godep:
	go get github.com/tools/godep

test:
	godep go test ./...

install:
	godep go install ./...

goose:
	go get bitbucket.org/liamstask/goose/cmd/goose
