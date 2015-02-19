module Abalone {
	export interface Hex {
		q: number;
		r: number;
	}
	
	export function hexIndex(hexes: Hex[], x: Hex): number {
		for (var i=0; i<hexes.length; i++) {
			if (hexes[i].q === x.q && hexes[i].r === x.r) return i;
		}
		return -1;
	}

	export function hexagonalGrid(hexesOnEdge: number): Hex[] {
		var out: Hex[] = [];
		for (var r = 0; r < hexesOnEdge; r++) {
			out = out.concat(ring(r));
		}
		return out;
	}

	export function ring(rad: number): Hex[] {
		var current: Hex = {q: -rad, r: 0};
		if (rad === 0) return [current];
		var out = [];
		directions.forEach((d) => {
			for (var r = 0; r < rad; r++) {
				current = adjacent(current, d);
				out.push(current);
			}
		});
		return out;
	}

	export function nearbyDirections(d: Direction): Direction[] {
		var idx = directions.indexOf(d);
		var nearby = [-1, 0, 1].map((x) => (x + idx + 6) % 6);
		return nearby.map((i) => directions[i]);
	}

	export function findDirection(p1: Hex, p2: Hex) {
		var q1 = p1.q;
		var r1 = p1.r;
		var q2 = p2.q;
		var r2 = p2.r;
		if (q1 === q2 && r1 === r2) {
			return null;
		} else if (q1 === q2) {
			return r1 < r2 ? Direction.BotRight : Direction.TopLeft;
		} else if (r1 === r2) {
			return q1 < q2 ? Direction.MidRight : Direction.MidLeft;
		} else if (r1 + q1 === r2 + q2) {
			return q1 < q2 ? Direction.TopRight : Direction.BotLeft;
		} else {
			return null;
		}
	}

	export function dist2(x1: Hex, x2: Hex) {
		return Math.abs(x1.q - x2.q) + Math.abs(x1.r - x2.r) + Math.abs(x1.q + x1.r - x2.q - x2.r);
	}

	export function dist(x1: Hex, x2: Hex) {
		return Math.round(dist2(x1, x2) / 2)
	}

	export function onBoard(b: Board, p: Hex): boolean {
		return dist2(p, {q: 0, r: 0}) < b.edgeLength * 2;
	}

	export function opposite(d: Direction): Direction {
		switch(d) {
		case Direction.TopRight: return Direction.BotLeft
		case Direction.MidRight: return Direction.MidLeft
		case Direction.BotRight: return Direction.TopLeft
		case Direction.BotLeft : return Direction.TopRight
		case Direction.MidLeft : return Direction.MidRight
		case Direction.TopLeft : return Direction.BotRight	
		}
	}

	export function colinear(d1: Direction, d2: Direction): boolean {
		return d1 === d2 || d1 === opposite(d2);
	}

	export function adjacent(position: Hex, d: Direction): Hex {
		var q = position.q;
		var r = position.r;
		switch (d) {
			case Direction.TopRight: return {q: q+1, r: r-1};
			case Direction.MidRight: return {q: q+1, r: r  };
			case Direction.BotRight: return {q: q  , r: r+1};
			case Direction.BotLeft:  return {q: q-1, r: r+1};
			case Direction.MidLeft:  return {q: q-1, r: r  };
			case Direction.TopLeft:  return {q: q  , r: r-1};
		}
	}

}
