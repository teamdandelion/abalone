/// <reference path="hex.ts" />
/// <reference path="player.ts" />
/// <reference path="direction.ts" />

module Abalone {
export module Engine {
	export interface Segment {
		basePos: Hex;
		orientation: Direction;
		segLength: number;
		player: Player;
	}


	export function segPieces(s: Segment): Hex[] {
		if (s == null) return [];
		var front = s.basePos;
		var pieces = [front];
		for (var i=0; i<s.segLength-1; i++) {
			front = adjacent(front, s.orientation);
			pieces.push(front);
		}
		return pieces;
	}
}
}