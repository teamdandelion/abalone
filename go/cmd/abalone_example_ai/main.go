package main

import (
	"github.com/danmane/abalone/go/abalone"
	"github.com/danmane/abalone/go/ai"
)

func main() {
	agent := ai.AgentInfo{
		Owner: "btc",
		Taunts: []string{
			"U MAD BRO?",
			"If you took an IQ test, the results would be negative.",
			"I don't know what makes you so dumb but it really works",
			"If brains were taxed, you'd get a rebate.",
			"Zombies eat brains. You’re safe.",
		},
	}
	ai.Play(agent, func(s abalone.Game) abalone.Game {
		return s
	})
}
