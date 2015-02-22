package game

import (
	"bytes"
	"encoding/json"
	"testing"
)

func Benchmark_segments(b *testing.B) {
	for i := 0; i <= b.N; i++ {
		Standard.segments()
	}
}

func Test_segments(t *testing.T) {
	numSegs := len(Standard.segments())
	if numSegs != 55 {
		t.Error("number segments in standard game: expected 55, got", numSegs)
	}
}

func Benchmark_futures(b *testing.B) {
	b.ReportAllocs()
	for n := 0; n <= b.N; n++ {
		Standard.Futures()
	}
}

func Benchmark_State_Update(b *testing.B) {
	ms := Standard.moves()
	for n := 0; n <= b.N; n++ {
		for _, m := range ms {
			Standard.Update(&m)
		}
	}
}

func TestFuturesAreValid(t *testing.T) {
	futures := Standard.Futures()
	for _, f := range futures {
		if !Standard.ValidFuture(f) {
			t.Error("expected future ", f, "to be a valid future for game", Standard)
		}
	}
}

func TestGameMarshalJSON(t *testing.T) {
	g := Standard
	var buf bytes.Buffer
	if err := json.NewEncoder(&buf).Encode(&g); err != nil {
		t.Fatal(err)
	}

	data := buf.Bytes()
	canonical_encoded := `{"board":{"whitePositions":[{"q":0,"r":2},{"q":-3,"r":4},{"q":-2,"r":2},{"q":-1,"r":3},{"q":0,"r":3},{"q":0,"r":4},{"q":1,"r":3},{"q":-4,"r":4},{"q":-3,"r":3},{"q":-2,"r":3},{"q":-1,"r":2},{"q":-4,"r":3},{"q":-2,"r":4},{"q":-1,"r":4}],"blackPositions":[{"q":3,"r":-3},{"q":1,"r":-2},{"q":2,"r":-4},{"q":2,"r":-3},{"q":4,"r":-3},{"q":0,"r":-4},{"q":3,"r":-4},{"q":4,"r":-4},{"q":-1,"r":-3},{"q":1,"r":-4},{"q":2,"r":-2},{"q":0,"r":-3},{"q":0,"r":-2},{"q":1,"r":-3}],"edgeLength":5},"nextPlayer":"white","movesRemaining":1000,"marblesPerMove":3,"lossThreshold":8}`
	var canonical_decoded State
	if err := json.NewDecoder(bytes.NewBufferString(canonical_encoded)).Decode(&canonical_decoded); err != nil {
		t.Fatal(err)
	}
	if !g.eq(&canonical_decoded) {
		t.Error("standard game did not match canonical serialized game")
	}
	var gprime State
	if err := json.NewDecoder(bytes.NewBuffer(data)).Decode(&gprime); err != nil {
		t.Fatal(err)
	}
	if !g.eq(&gprime) {
		t.Error("game changed by serialization operation")
		t.Log("before", g)
		t.Log("serialized", buf.String())
		t.Log("after", gprime)
	}
}
