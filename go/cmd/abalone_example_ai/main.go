package main

import "github.com/danmane/abalone/go/abalone"

func main() {
	agent := abalone.AgentInfo{
		Owner: "btc",
		Taunts: []string{
			"U MAD BRO?",
			"If you took an IQ test, the results would be negative.",
			"I don't know what makes you so dumb but it really works",
			"If brains were taxed, you'd get a rebate.",
			"Zombies eat brains. You’re safe.",
		},
	}
	abalone.Play(agent, func(s abalone.GameState) abalone.GameState {
		return s
	})

}
