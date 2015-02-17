module Abalone {
	export enum Player {White, Black}
	export function next(p: Player) {
		return (p === Player.White) ? Player.Black : Player.White;
	}

	export function player2str(p: Player): string {
		return p === Player.White ? "White" : "Black"
	}
	export function str2player(s: String): Player {
		switch (s) {
			case "White":
			return Player.White;
			case "Black": 
			return Player.Black;
			default:
			return null;
		}
	}
}