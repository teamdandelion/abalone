package serialize

import (
	"bytes"
	"encoding/json"
	"testing"

	pio "code.google.com/p/gogoprotobuf/io"
	proto "github.com/danmane/abalone/go/experimental/serialize/proto"
	"github.com/danmane/abalone/go/game"
)

const (
	bytesPerStateJSON  = 562
	bytesPerStateProto = 184
)

func TestGameStateProtoSerialization(t *testing.T) {
	state := game.Standard
	var jsonbuf bytes.Buffer
	if err := json.NewEncoder(&jsonbuf).Encode(&state); err != nil {
		t.Fatal(err)
	}
	if jsonbuf.Len() != bytesPerStateJSON {
		t.Fatalf("game states are now %d bytes", jsonbuf.Len())
	}
	protostate := toProto(state)
	var protobuf bytes.Buffer
	w := pio.NewDelimitedWriter(&protobuf)

	if err := w.WriteMsg(&protostate); err != nil {
		t.Fatal(err)
	}
	if protobuf.Len() != bytesPerStateProto {
		t.Fatal("proto encoding is now %d bytes", protobuf.Len())
	}
}

func toProto(state game.State) proto.State {
	var b proto.Board
	l := uint32(state.Board.EdgeLength)
	b.EdgeLength = &l
	for hex, _ := range state.Board.BlackPositions {
		q := int32(hex.Q)
		r := int32(hex.R)
		b.BlackPositions = append(b.BlackPositions, &proto.Hex{
			Q: &q,
			R: &r,
		})
	}
	for hex, _ := range state.Board.WhitePositions {
		q := int32(hex.Q)
		r := int32(hex.R)
		b.WhitePositions = append(b.WhitePositions, &proto.Hex{
			Q: &q,
			R: &r,
		})
	}
	mr := uint32(state.MovesRemaining)
	mpm := uint32(state.MarblesPerMove)
	lt := uint32(state.LossThreshold)
	p := func() proto.Player {
		switch state.NextPlayer {
		case game.Black:
			return proto.Player_BLACK
		case game.White:
			return proto.Player_WHITE
		default:
			return proto.Player_BLACK
		}
	}()
	return proto.State{
		Board:          &b,
		NextPlayer:     &p,
		MovesRemaining: &mr,
		MarblesPerMove: &mpm,
		LossThreshold:  &lt,
	}
}
