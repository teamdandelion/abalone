package operator

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

func TestConvertMillisecondToDuration(t *testing.T) {
	d := 2 * time.Second
	count := toMillisecondCount(d)
	got := time.Duration(count) * time.Millisecond
	if got != d {
		t.Fatalf("expected %d, got %d", d, count)
	}
}
