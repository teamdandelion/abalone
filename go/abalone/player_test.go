package abalone

import (
	"bytes"
	"encoding/json"
	"testing"
)

func TestPlayerJSONRepresentation(t *testing.T) {
	p := White
	var buf bytes.Buffer
	json.NewEncoder(&buf).Encode(p)

	expected := "\"white\"\n"
	if buf.String() != expected {
		t.Fatalf("error encoding player: expected %s, got %s", expected, buf.String())
	}
}
