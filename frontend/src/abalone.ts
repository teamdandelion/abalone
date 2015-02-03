
module Abalone {
	export enum Player {White, Black}
	export enum Outcome {WhiteWins, BlackWins, TieGame}
	export enum Direction {TopRight, MidRight, BotRight, TopLeft, MidLeft, BotLeft}
	export interface Game {
		board: Board;
		nextPlayer: string;
		movesRemaining: number;
		marblesPerMove: number;
		lossThreshold: number;
	}

	export interface Board {
		whitePositions: [number, number][];
		blackPositions: [number, number][];
		boardRadius: number;
	}

	export interface Segment {
		base: [number, number];
	}

	export function winner(g: Game): Outcome {
		if (gameOver(g)) {
			var w = g.board.whitePositions.length;
			var b = g.board.blackPositions.length;
			if (w > b) return Outcome.WhiteWins;
			if (b > w) return Outcome.BlackWins;
			return Outcome.TieGame;
		}
		return null;
	}

	export function gameOver(g: Game): boolean {
		var b = g.board;
		return  g.movesRemaining <= 0 || Math.min(b.whitePositions.length, b.blackPositions.length)  <= g.lossThreshold;
	}

	function standardBoard() {
		return <Board> <any> {
			boardRadius   : 5, 
			whitePositions:[[-4,3],[-4,4],[-3,3],[-3,4],[-2,2],[-2,3],[-2,4],[-1,2],[-1,3],[-1,4],[0,2],[0,3],[0,4],[1,3]], 
			blackPositions:[[-1,-3],[0,-4],[0,-3],[0,-2],[1,-4],[1,-3],[1,-2],[2,-4],[2,-3],[2,-2],[3,-4],[3,-3],[4,-4],[4,-3]]
		}
	}

	export function standardGame(): Game {
		return {
			lossThreshold : 8,
			marblesPerMove: 3,
			movesRemaining: 200,
			nextPlayer    : "White",
			board         : standardBoard()
		}
	}	
}
