package operator

import (
	"fmt"
	"math/rand"
	"sync"
)

type PortScheduler struct {
	mu              sync.Mutex
	numManagedPorts int
	basePort        int
	occupiedPorts   map[int]struct{}
}

func (ps *PortScheduler) GetPort() (int, error) {
	ps.mu.Lock()
	defer ps.mu.Unlock()
	for _, i := range rand.Perm(ps.numManagedPorts) {
		potentialport := ps.basePort + i
		if _, inUse := ps.occupiedPorts[potentialport]; !inUse {
			ps.occupiedPorts[potentialport] = struct{}{}
			return potentialport, nil
		}
	}
	return 0, fmt.Errorf("Ports exhausted")
}

func (ps *PortScheduler) ReleasePort(port int) {
	ps.mu.Lock()
	defer ps.mu.Unlock()
	delete(ps.occupiedPorts, port)
}

func NewScheduler(basePort, numPorts int) PortScheduler {
	return PortScheduler{
		numManagedPorts: numPorts,
		basePort:        basePort,
		occupiedPorts:   make(map[int]struct{}),
	}
}
