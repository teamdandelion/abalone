package main

import (
	"github.com/danmane/abalone/go/game"
	"github.com/danmane/abalone/go/quickstart"
)

func main() {
	agent := quickstart.AgentInfo{
		Owner: "btc",
		Taunts: []string{
			"U MAD BRO?",
			"If you took an IQ test, the results would be negative.",
			"I don't know what makes you so dumb but it really works",
			"If brains were taxed, you'd get a rebate.",
			"Zombies eat brains. You’re safe.",
		},
	}
	quickstart.Play(agent, func(s game.State) game.State {
		f := s.Futures()
		if len(f) == 0 {
			panic("Ah! There are no future states. Why'd the server send this to me? =( ")
		}
		return f[0]
	})
}
