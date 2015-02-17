package abalone

import (
	"bytes"
	"encoding/json"
	"testing"
)

func Benchmark_segments(b *testing.B) {
	for i := 0; i <= b.N; i++ {
		StandardGame.segments()
	}
}

func Test_segments(t *testing.T) {
	numSegs := len(StandardGame.segments())
	if numSegs != 55 {
		t.Error("number segments in standard game: expected 55, got", numSegs)
	}
}

func Benchmark_futures(b *testing.B) {
	b.ReportAllocs()
	for n := 0; n <= b.N; n++ {
		StandardGame.Futures()
	}
}

func TestGameMarshalJSON(t *testing.T) {
	g := StandardGame
	var buf bytes.Buffer
	if err := json.NewEncoder(&buf).Encode(&g); err != nil {
		t.Fatal(err)
	}

	data := buf.Bytes()
	var gprime Game
	if err := json.NewDecoder(bytes.NewBuffer(data)).Decode(&gprime); err != nil {
		t.Fatal(err)
	}
	if !g.eq(gprime) {
		t.Error("game changed by serialization operation")
		t.Log("before", g)
		t.Log("serialized", buf.String())
		t.Log("after", gprime)
	}
}
