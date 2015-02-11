module Abalone {
	export module Hex {
		export var directions = [Direction.TopRight, Direction.MidRight, Direction.BotRight, Direction.BotLeft, Direction.MidLeft, Direction.TopLeft];

		export function hexagonalGrid(hexesOnEdge: number): number[][] {
			var out: number[][] = [];
			for (var r = 0; r < hexesOnEdge; r++) {
				out = out.concat(ring(r));
			}
			return out;
		}

		export function ring(rad: number): number[][] {
			var current: number[] = [-rad, 0];
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

		export function dist2(x1: number[], x2: number[]) {
			var q1 = x1[0];
			var q2 = x2[0];
			var r1 = x1[1];
			var r2 = x2[1];
			return Math.abs(q1 - q2) + Math.abs(r1 - r2) + Math.abs(q1 + r1 - q2 - r2);
		}

		export function onBoard(b: Board, p: number[]): boolean {
			return dist2(p, [0,0]) < b.boardRadius * 2;
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

		export function adjacent(position: number[], d: Direction): number[] {
			var q = position[0];
			var r = position[1];
			switch (d) {
				case Direction.TopRight: return [q+1, r-1];
				case Direction.MidRight: return [q+1, r  ];
				case Direction.BotRight: return [q  , r+1];
				case Direction.BotLeft:  return [q-1, r+1];
				case Direction.MidLeft:  return [q-1, r  ];
				case Direction.TopLeft:  return [q  , r-1];
			}
		}

	}
}
