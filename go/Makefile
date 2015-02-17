all:
	# nop

vendor: godep
	godep save ./...

godep:
	go get github.com/tools/godep

test:
	godep go test ./...

install:
	godep go install ./...
