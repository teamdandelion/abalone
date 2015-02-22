# TODO(brian): add proto tasks
all: game_state.pb.go

game_state.pb.go: game_state.proto
	protoc --gogo_out=. --proto_path=../../../../../:/usr/local/opt/protobuf/include:. $<

clean:
	rm game_state.pb.go
