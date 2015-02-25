module Abalone {
export module Engine {
	export enum Player {White, Black}
	export function next(p: Player) {
		return (p === Player.White) ? Player.Black : Player.White;
	}

	export function player2str(p: Player): string {
		return p === Player.White ? "white" : "black"
	}
	export function str2player(s: String): Player {
		switch (s) {
			case "white":
			return Player.White;
			case "black": 
			return Player.Black;
			default:
			return null;
		}
	}
}
}