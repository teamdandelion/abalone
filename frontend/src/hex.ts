module Abalone {
	export module Hex {
		export function hexagonalGrid(hexesOnEdge: number): [number, number][] {
			var out: [number, number][] = [];
			for (var r=0; r<hexesOnEdge; r++) {
				out = out.concat(ring(r));
			}
			return out;
		}

		function ring(rad: number): [number, number][] {
			var current: [number, number] = [-rad, 0];
			if (rad == 0) return [current];
			var out = [];
			directions.forEach((d) => {
				for (var r=0; r<rad; r++) {
					current = adjacent(current, d);
					out.push(current);
				}
			});
			return out;
		}

		export function adjacent(position: [number, number], d: Direction): [number, number] {
			var q = position[0];
			var r = position[1];
			switch (d) {
				case Direction.TopRight: return [q+1, r-1]
				case Direction.MidRight: return [q+1, r  ]
				case Direction.BotRight: return [q  , r+1]
				case Direction.BotLeft:  return [q-1, r+1]
				case Direction.MidLeft:  return [q-1, r  ]
				case Direction.TopLeft:  return [q  , r-1]
			}
		}

		var directions = [Direction.TopRight, Direction.MidRight, Direction.BotRight, Direction.TopLeft, Direction.MidLeft, Direction.BotLeft];
	}
}