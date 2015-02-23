package operator

import (
	"fmt"
	"sync"
)

type PortScheduler struct {
	numPortsAvailable int
	offset            int
	occupiedPorts     map[int]struct{}
	mu                sync.Mutex
}

func (ps *PortScheduler) GetPort() (int, error) {
	ps.mu.Lock()
	defer ps.mu.Unlock()
	for i := ps.offset; i < ps.offset+ps.numPortsAvailable; i++ {
		if _, ok := ps.occupiedPorts[i]; !ok {
			ps.occupiedPorts[i] = struct{}{}
			return i, nil
		}
	}
	return 0, fmt.Errorf("Ports exhausted")
}

func (ps *PortScheduler) ReleasePort(port int) {
	ps.mu.Lock()
	defer ps.mu.Unlock()
	delete(ps.occupiedPorts, port)
}

func NewScheduler(offset, numPorts int) PortScheduler {
	return PortScheduler{
		numPortsAvailable: numPorts,
		offset:            offset,
		occupiedPorts:     make(map[int]struct{}),
	}
}
