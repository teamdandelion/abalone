module Abalone {
    export class Renderer {
        private svg: D3.Selection;
        private board: D3.Selection;
        private whitePieces: D3.Selection;
        private blackPieces: D3.Selection;
        private overlay: D3.Selection;
        private grid: D3.Selection;
        private hexesOnEdge: number;
        private height: number;
        private width: number;
        private hexSize: number;

        constructor(svg: D3.Selection, width: number, height: number) {
            this.svg = svg;
            this.hexesOnEdge = 5;
            this.overlay = this.svg.append("g").classed("overlay", true);
            this.board = this.svg.append("g").classed("board", true);
            this.grid = this.board.append("g").classed("grid", true);
            this.whitePieces = this.board.append("g").classed("white", true);
            this.blackPieces = this.board.append("g").classed("black", true);

            this.resize(width, height);
        }

        public resize(width, height) {
            this.width = width;
            this.height = height;
            this.hexSize = Math.min(width, height) / this.hexesOnEdge / 4;
            this.drawBoard();
            // this.drawPieces();
            // this.drawOverlay();
        }

        private qr2xy(q: number, r: number): [number, number] {
            return [
                this.hexSize * Math.sqrt(3) * (q + r/2) + this.width/2, 
                this.hexSize * 3/2 * r + this.height/2
                ];
        }

        public drawGame(g: Game) {
            this.drawPieces(g.board);
        }

        private drawPieces(b: Board) {
            this.addPieces(this.whitePieces, b.whitePositions);
            this.addPieces(this.blackPieces, b.blackPositions);
        }

        private addPieces(selection: D3.Selection, pieces: [number,number][]) {
            var xf = (d,i) => this.qr2xy(d[0], d[1])[0];
            var yf = (d,i) => this.qr2xy(d[0], d[1])[1];
            var update = selection
                .selectAll("circle")
                .data(pieces);
            update
                .enter()
                    .append("circle")
                    .attr("cx", xf)
                    .attr("cy", yf)
                    .attr("r",  this.hexSize/2);
            update
                .exit().remove();

        }

        private drawOverlay() {

        }

        private drawBoard() {
            this.grid.selectAll("polygon").remove();
            var hexes = Hex.hexagonalGrid(this.hexesOnEdge);
            hexes.forEach((h) => {
                this.drawHex(this.grid, h[0], h[1]);
            });
        }

        private drawHex(container: D3.Selection, q: number, r: number) {
            var rad = this.hexSize;
            var xy = this.qr2xy(q,r);
            var x = xy[0];
            var y = xy[1];
            var points = hexPointString(rad, x, y);
            container.append("polygon").attr("points", points).classed("hex", true);
        }
    }


    function hexPointString(size, x, y) {
        var s = "";
        for (var i = 0; i<6; i++) {
            s += hexCorner(x, y, size, i);
            s += " ";
        }
        return s;
    }

    function hexCorner(x, y, size, i) {
        var angle = 2 * Math.PI / 6 * (i + 0.5);
        return [x + size * Math.cos(angle), y + size * Math.sin(angle)];
    }
}
