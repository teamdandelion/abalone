module Abalone {
export module Frontend {
	export interface PlayerAgent {
		play(g: Engine.Game, cb: (g: Engine.Game) => void): void;
	}
}
}