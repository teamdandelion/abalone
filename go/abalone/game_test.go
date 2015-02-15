package abalone

import "testing"

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
