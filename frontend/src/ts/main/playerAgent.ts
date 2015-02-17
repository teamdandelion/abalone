module Main {
	export interface PlayerAgent {
		play(g: Abalone.Game, cb: (g: Abalone.Game) => void): void;
	}
}
