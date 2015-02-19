package main

import (
	"testing"
	"time"
)

func TestMillisecondConversion(t *testing.T) {
	d := 2 * time.Second
	var expected int64 = 2000
	if count := toMillisecondCount(d); count != expected {
		t.Fatalf("expected %d, got %d", expected, count)
	}
}
