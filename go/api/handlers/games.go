package handlers

import (
	"encoding/json"
	"log"
	"net/http"

	api "github.com/danmane/abalone/go/api"
	"github.com/danmane/abalone/go/game"
	"github.com/danmane/abalone/go/operator"
)

// RunGamesHandler runs a game between two remote player instances
func RunGamesHandler(ds *api.Services) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		req := struct {
			BlackPort string
			WhitePort string
		}{}
		if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		whiteAgent := operator.RemotePlayerInstance{
			APIPlayer: api.Player{},
			Port:      req.WhitePort,
		}
		blackAgent := operator.RemotePlayerInstance{
			APIPlayer: api.Player{},
			Port:      req.BlackPort,
		}
		result := operator.ExecuteGame(&whiteAgent, &blackAgent, operator.Config{
			Start: game.Standard,
			Limit: api.DefaultMoveLimit,
		})
		log.Println(result.Outcome)
		log.Println(result.VictoryReason)
		if err := json.NewEncoder(w).Encode(result); err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
	}
}
