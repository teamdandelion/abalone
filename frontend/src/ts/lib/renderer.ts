module Abalone {
    export class Renderer implements PlayerAgent {
        private svg: D3.Selection;
        private board: D3.Selection;
        private whitePieces: D3.Selection;
        private blackPieces: D3.Selection;
        private overlay: D3.Selection;
        private eventLayer: D3.Selection;
        private grid: D3.Selection;
        private hexesOnEdge: number;
        private height: number;
        private width: number;
        private hexSize: number;

        constructor(svg: D3.Selection, width: number, height: number, hexesOnEdge=5) {
            this.svg = svg;
            this.hexesOnEdge = hexesOnEdge;
            this.eventLayer = this.svg.append("g");
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

        private addPieces(selection: D3.Selection, pieces: number[][]) {
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

        private hexFromXY(x: number, y: number): number[] {
            return [0,0];
        }

        private drawOverlay(segment: Segment, isDragging: boolean) {

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

        public play(g: Game, cb: (g: Game) => void): void {
            var selectedPieces: Segment;

            var disabled = false;
            var dragInProgress = false;
            var originHex = null;

            var finish = (m: Move) => {
                this.drawOverlay(null, false);
                disabled = true;
                cb(update(g, m));
            }

            var dragstart = () => {
                if (disabled) return;
                var location = d3.mouse(this.eventLayer.node());
                originHex = this.hexFromXY(location[0], location[1]);
                if (selectedPieces !== null) {
                    var move = generateMove(selectedPieces, originHex);
                    if (move != null && isValid(g, move)) {
                        finish(move);
                    } else {
                        selectedPieces = null;
                    }
                } else {
                    if ((selectedPieces = getSegment(g, originHex)) !== null) {
                        dragInProgress = true;
                    }
                }
            }

            var drag = () => {
                if (disabled) return;
                var currentHex = this.hexFromXY(d3.event.x, d3.event.y);
                selectedPieces = getSegment(originHex, currentHex);
                this.drawOverlay(selectedPieces, true);
            }

            var dragend = () => {
                if (disabled) return;
                var currentHex = this.hexFromXY(d3.event.x, d3.event.y);
                selectedPieces = getSegment(originHex, currentHex);
                this.drawOverlay(selectedPieces, false);
            }

            this.eventLayer.call(
                d3.behavior.drag()
                    .on("dragstart", dragstart)
                    .on("drag", drag)
                    .on("dragend", dragend)
            )
        }

    }

    export function generateMove(s: Segment, target: number[]): Move {
        var possibilities = adjacentHexDirs(s);
        var foundDir: Direction;
        possibilities.forEach((hd) => {
            var hex = hd[0];
            var dir = hd[1];
            if (hex.toString() === target.toString()) {
                foundDir = dir;
            }
        });
        var move = foundDir != null ? {segment: s, direction: foundDir} : null;
        return move;
    }

    function vanguard(pos: number[], d: Direction): [number[], Direction][] {
        return <any> Hex.nearbyDirections(d).map((dir) => [Hex.adjacent(pos, dir), dir]);
    }

    function adjacentHexDirs(s: Segment): [number[], Direction][] {
        if (s.orientation == null) {
            return <any> Hex.directions.map((d) => [Hex.adjacent(s.basePos, d), d]);
        } else {
            var front = vanguard(s.basePos, Hex.opposite(s.orientation));
            var back = vanguard(_.last(segPieces(s)), s.orientation);
            return front.concat(back);
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
