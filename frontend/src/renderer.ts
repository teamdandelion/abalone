module Abalone {
	class Renderer {
		private svg: D3.Selection;
		private board: D3.Selection;
		private whitePieces: D3.Selection;
		private blackPieces: D3.Selection;
		private overlay: D3.Selection;
		private hexesOnEdge: number;

		constructor(svg: D3.Selection, width: number, height: number) {
			this.svg = svg;
			this.hexesOnEdge = 5;
			this.board = this.svg.append("g").classed("board", true);
			this.whitePieces = this.svg.append("g").classed("white", true);
			this.blackPieces = this.svg.append("g").classed("black", true);
			this.overlay = this.svg.append("g").classed("overlay", true);

			this.resize(width, height)
		}

		public resize(width, height) {
			this.width = width;
			this.height = height;
			this.hexRadius = Math.min(width, height) / this.hexesOnEdge / 4;
			this.drawBoard();
			this.drawPieces();
			this.drawOverlay();
		}

		public hexPosition(q, r) {
			return [this.size * Math.sqrt(3) * (q + r/2), this.size * 3/2 * r]
		}

		public drawBoard() {
			this.board.select(".grid").remove();
			var grid = this.board.append("g").classed("grid", true);
			var hexes = Hex.hexagonalGrid(this.hexesOnEdge);
			hexes.forEach((h) => {
				this.drawHex(grid, h[0], h[1]);
			});
		}

		public drawHex(container: D3.Selection, q: number, r: number) {
			var rad = this.hexRadius();
			var xy = this.convertCoordinate(q,r);
			var x = xy[0];
			var y = xy[1];
			var points = hexPointString(rad, x, y);
			container.append("polygon").attr("points", points).classed("hex", true);
		}
	}


	function hexPointString(radius, x, y) {
		var s = "";
		for (var i = 0; i<6; i++) {
			s += hexCorner(x, y, radius, i);
			s += " "
		}
		return s;
	}

	function hexCorner(x, y, radius, i) {
		var angle = 2 * Math.PI / 6 * (i + 0.5);
		return [x + size * Math.cos(angle), y + size * Math.sin(angle)];
	}
}