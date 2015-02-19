/// <reference path="hex.ts" />
/// <reference path="player.ts" />

module Abalone {
	export interface Board {
		whitePositions: Hex[];
		blackPositions: Hex[];
		edgeLength: number;
	}

	export function free(b: Board, x: Hex): boolean {
		return owner(b,x) == null;
	}

	export function owner(b: Board, x: Hex): Player {
		if (hexIndex(b.whitePositions, x) !== -1) return Player.White;
		if (hexIndex(b.blackPositions, x) !== -1) return Player.Black;
		return null;
	}

	export function getPieces(b: Board, p: Player): Hex[] {
		if (p === Player.White) {
			return b.whitePositions;
		} else {
			return b.blackPositions;
		}
	}

	export function tuplesToHexes(tups: number[][]): Hex[] {
		return tups.map((t: number[]) => {
			return {q: t[0], r: t[1]};
		});
	}

	export function standardBoard() {
		return <Board> <any> {
			edgeLength   : 5, 
			whitePositions: tuplesToHexes([
				[-4,3],[-4,4],[-3,3],[-3,4],[-2,2],
				[-2,3],[-2,4],[-1,2],[-1,3],[-1,4],
				[0,2],[0,3],[0,4],[1,3]
				]), 
			blackPositions: tuplesToHexes([
				[-1,-3],[0,-4],[0,-3],[0,-2],[1,-4],
				[1,-3],[1,-2],[2,-4],[2,-3],[2,-2],
				[3,-4],[3,-3],[4,-4],[4,-3]
				])
		}
	}

	export function smallBoard() {
		return <Board> <any> {
			edgeLength: 2,
			whitePositions: tuplesToHexes([
				[-1, 1], [0,1], [0,0]
				]),
			blackPositions: tuplesToHexes([
				[0,-1], [1,-1]
				])
		}
	}
}