package abalone

import (
	"bytes"
	"encoding/json"
	"testing"
)

func TestPlayerMarshalJSON(t *testing.T) {
	p := White
	var buf bytes.Buffer
	json.NewEncoder(&buf).Encode(p)

	expected := "\"white\"\n"
	if buf.String() != expected {
		t.Fatalf("error encoding player: expected %s, got %s", expected, buf.String())
	}
}

func TestPlayerUnmarshalJSON(t *testing.T) {
	tests := []struct {
		string
		Player
	}{
		{"\"white\"", White},
		{"\"white\"\n", White},
	}
	for _, test := range tests {
		buf := bytes.NewBufferString(test.string)
		var p Player
		json.NewDecoder(buf).Decode(&p)
		if test.Player != p {
			t.Fatalf("expected player %s, got %s from JSON: %s", test.Player, p, test.string)
		}
	}
}
