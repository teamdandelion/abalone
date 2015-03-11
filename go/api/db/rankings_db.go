package db

import (
	"fmt"

	"github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/game"
	"github.com/danmane/abalone/go/rankings"
)

type rankingsDB struct {
	*resources
}

func (s *rankingsDB) List() ([]*api.Ranking, error) {

	// first, fetch requisite data from DB

	games, err := s.Services.Games.List()
	if err != nil {
		return nil, err
	}
	players, err := s.Services.Players.List()
	if err != nil {
		return nil, err
	}
	users, err := s.Services.Users.List()
	if err != nil {
		return nil, err
	}

	// then compute rankings based on the above data

	var consideredGames []*api.Game // games included in ranking computation
	for _, g := range games {
		if g.Status == string(api.GameScheduled) {
			continue
		}
		consideredGames = append(consideredGames, &g)
	}

	var results []rankings.Result
	for _, g := range consideredGames {
		o, err := g.Outcome()
		if err != nil {
			return nil, err // if we've fucked this up, better to let the computation fail early
		}
		results = append(results, rankings.Result{
			WhiteID: g.WhiteId,
			BlackID: g.BlackId,
			Outcome: o,
		})
	}

	var playerIDs []int64
	for _, p := range players {
		playerIDs = append(playerIDs, p.ID)
	}

	rankings, err := rankings.RateGames(playerIDs, results)
	if err != nil {
		return nil, err
	}

	// with rankings in hand, transform into the API representation

	playersByID := toPlayerMap(players)                           // for player name lookups
	usersByAuthorID := toAuthorIndex(users)                       // for author name lookups
	winlossByPlayerID := computeWinLoss(players, consideredGames) // for win/loss info

	var out []*api.Ranking
	for _, r := range rankings {

		player := playersByID[r.PlayerID]
		author := usersByAuthorID[player.AuthorId].Name
		wl := winlossByPlayerID[player.ID]

		out = append(out, &api.Ranking{
			Rank:   r.Rank,
			Rating: r.Rating.Mean,
			Player: fmt.Sprintf("%s v%d.%d", player.Name, player.Version, player.ID),
			Author: author,
			Wins:   wl.Wins,
			Losses: wl.Losses,
		})
	}
	return out, nil
}

func toPlayerMap(players []api.Player) map[int64]api.Player {
	m := make(map[int64]api.Player)
	for _, p := range players {
		m[p.ID] = p
	}
	return m
}

func toAuthorIndex(users []api.User) map[int64]api.User {
	m := make(map[int64]api.User)
	for _, u := range users {
		m[u.ID] = u
	}
	return m
}

type winloss struct {
	Wins   int
	Losses int
}

func computeWinLoss(players []api.Player, games []*api.Game) map[int64]*winloss {
	wl := make(map[int64]*winloss)
	for _, p := range players {
		wl[p.ID] = new(winloss)
	}
	for _, g := range games {
		var winner, loser int64
		o, err := g.Outcome()
		if err != nil {
			continue
		}
		switch o {
		case game.BlackWins:
			winner, loser = g.BlackId, g.WhiteId
		case game.WhiteWins:
			winner, loser = g.WhiteId, g.BlackId
		default:
			continue
		}
		wl[winner].Wins++
		wl[loser].Losses++
	}
	return wl
}

var _ api.RankingsService = (*rankingsDB)(nil)
