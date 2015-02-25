package operator

import (
	"testing"

	"github.com/jbenet/go-ipfs/thirdparty/iter"
)

func TestGetPort(t *testing.T) {

	tests := []struct {
		FirstPort int
		NumPorts  int
	}{
		{10, 100},
		{1000, 1},
	}
	for _, test := range tests {
		scheduler := NewScheduler(test.FirstPort, test.NumPorts)
		allocated := make(map[int]struct{})
		for range iter.N(test.NumPorts) {
			p, err := scheduler.GetPort()
			if err != nil {
				t.Fatal(err)
			}
			allocated[p] = struct{}{}
		}
		_, err := scheduler.GetPort()
		if err == nil {
			t.Fatal("expected to run out of ports")
		}
		for port, _ := range allocated {
			scheduler.ReleasePort(port)
		}

		// allocate all ports again
		for range iter.N(test.NumPorts) {
			_, err := scheduler.GetPort()
			if err != nil {
				t.Fatal(err)
			}
		}
		if _, err := scheduler.GetPort(); err == nil {
			t.Fatal("expected to run out of ports")
		}
	}
}
