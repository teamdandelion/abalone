/// <reference path="hex.ts" />
/// <reference path="segment.ts" />
/// <reference path="direction.ts" />

module Abalone {
	export interface Move {
		segment: Segment;
		direction: Direction;
	}

	export function validMove(g: Game, m: Move): boolean {
		if (broadside(m)) {
			return segPieces(m.segment)
				.map((p) => adjacent(p, m.direction))
				.every((p) => free(g.board, p));
		} else {
			return inlineMoved(g.board, m) !== null;
		}
	}

	function inline(m: Move): boolean {
		return m.segment.orientation !== null && colinear(m.direction, m.segment.orientation)
	}

	export function broadside(m: Move): boolean {
		return !inline(m);
	}

	export function inlineMoved(b: Board, m: Move): Hex[] {
		if (broadside(m)) return null;
		var pieces = segPieces(m.segment);
		var attacked = m.segment.orientation === m.direction ? pieces[pieces.length-1] : pieces[0];
		var movedEnemyPieces = [];
		for (var i=0; i<m.segment.segLength; i++) {
			attacked = adjacent(attacked, m.direction);
			var controller = owner(b, attacked);
			if (controller == null) return movedEnemyPieces;
			if (controller === m.segment.player) return null;
			movedEnemyPieces.push(attacked);
		}
		return null;
	}
}