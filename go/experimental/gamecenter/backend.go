package gamecenter

import (
	"errors"
	"sort"
	"sync"

	"github.com/briantigerchow/gamecenter/abalone"
)

var errTODO = errors.New("TODO")

// TODO handle num states
type Game struct {
	Id     int
	White  Player
	Black  Player
	States []abalone.GameState
}

type Player struct {
	Id   int
	Name string
}

type GamesBackend interface {
	CreateGame(Game) (*Game, error)
	FindGame(gameID int) (*Game, error)
	ListGames(offset, limit int) []Game
	ListStates(gameID, offset, limit int) ([]abalone.GameState, error)
	CreateState(gameID int, state abalone.GameState, n int) error
}

type PostgresBackend struct {
	Host string
}

func (b *PostgresBackend) CreateGame(Game) (*Game, error)     { return nil, errTODO }
func (b *PostgresBackend) FindGame(gameID int) (*Game, error) { return nil, errTODO }
func (b *PostgresBackend) ListGames(offset, limit int) []Game { return nil }
func (b *PostgresBackend) ListStates(id, offset, limit int) ([]abalone.GameState, error) {
	return nil, errTODO
}
func (b *PostgresBackend) CreateState(gameID int, state abalone.GameState, n int) error {
	return errTODO
}

var _ GamesBackend = &PostgresBackend{}

type MemBackend struct {
	mu       sync.Mutex
	games    map[int]Game
	numGames int
}

func NewMemBackend() *MemBackend {
	return &MemBackend{games: make(map[int]Game)}
}

var _ GamesBackend = &MemBackend{}

func (b *MemBackend) CreateGame(g Game) (*Game, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	g.Id = b.numGames
	b.games[g.Id] = g
	b.numGames++
	created := b.games[g.Id]
	return &created, nil
}

func (b *MemBackend) ListGames(offset, limit int) []Game {
	if offset >= len(b.games) {
		return nil
	}
	b.mu.Lock()
	defer b.mu.Unlock()

	var games GameSlice
	for _, g := range b.games {
		games = append(games, g)
	}
	sort.Sort(games)

	end := offset + limit
	if offset+limit > len(games) {
		end = len(games)
	}
	return games[offset:end]
}

func (b *MemBackend) FindGame(gameID int) (*Game, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	g, found := b.games[gameID]
	if !found {
		return nil, errors.New("game not found")
	}
	return &g, nil
}

func (b *MemBackend) ListStates(id, offset, limit int) ([]abalone.GameState, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	g, ok := b.games[id]
	if !ok {
		return nil, errors.New("game does not exist")
	}
	var states []abalone.GameState
	for _, s := range g.States {
		states = append(states, s)
	}
	return states, nil
}

func (b *MemBackend) CreateState(gameID int, state abalone.GameState, n int) error {
	b.mu.Lock()
	defer b.mu.Unlock()
	game, ok := b.games[gameID]
	if !ok {
		return errors.New("game does not exist")
	}
	game.States[n] = state
	return nil
}

type GameSlice []Game

func (g GameSlice) Len() int {
	return len(g)
}

func (g GameSlice) Less(i, j int) bool {
	return g[i].Id < g[j].Id
}

func (g GameSlice) Swap(i, j int) {
	g[j], g[i] = g[i], g[j]
}
