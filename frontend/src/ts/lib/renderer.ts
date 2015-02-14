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
            this.overlay = this.svg.append("g").classed("overlay", true);
            this.board = this.svg.append("g").classed("board", true);
            this.grid = this.board.append("g").classed("grid", true);
            this.whitePieces = this.board.append("g").classed("white", true);
            this.blackPieces = this.board.append("g").classed("black", true);
            this.eventLayer = this.svg.append("rect")
                                    .attr({width: width, height: height})
                                    .style({fill: "black", opacity: 0})
                                    .classed("hitbox", true);

            this.resize(width, height);
        }

        public resize(width, height) {
            this.width = width;
            this.height = height;
            this.hexSize = Math.min(width, height) / this.hexesOnEdge / 4;
            this.drawBoard();
        }

        private qr2xy(q: number, r: number): [number, number] {
            return [
                this.hexSize * Math.sqrt(3) * (q + r/2) + this.width/2, 
                this.hexSize * 3/2 * r + this.height/2
                ];
        }

        public drawGame(g: Game) {
            this.drawPieces(g.board);
            var whiteIsNext = g.nextPlayer === Player.White;
            this.whitePieces.classed("faded", !whiteIsNext);
            this.blackPieces.classed("faded", whiteIsNext);


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
                    .append("circle");
            update
                .attr("cx", xf)
                .attr("cy", yf)
                .attr("r",  this.hexSize/2);
            update
                .exit().remove();

        }

        private hexFromXY(x: number, y: number): number[] {
            x = x - this.width/2;
            y = y - this.height/2;
            var q = (x * Math.sqrt(3)/3 - y/3) / this.hexSize;
            var r = y * 2/3 / this.hexSize;
            return this.hexRound(q,r);
        }

        private hexRound(q: number, r: number): number[] {
            var x = q;
            var z = r;
            var y = -x-z;
            
            var rx = Math.round(x);
            var ry = Math.round(y);
            var rz = Math.round(z);

            var xdiff = Math.abs(rx - x);
            var ydiff = Math.abs(ry - y);
            var zdiff = Math.abs(rz - z);

            if (xdiff > ydiff && xdiff > zdiff) {
                rx = -ry-rz;
            } else if (ydiff > zdiff) {
                ry = -rx-rz;
            } else {
                rz = -rx-ry;
            }
            return [rx, rz];
        }

        private drawOverlay(segment: Segment, isDragging: boolean, game: Game) {
            this.highlightHexes(segPieces(segment), "selected");
            if (segment != null && !isDragging) {
                this.highlightHexes(moveHexes(segment, game), "moves");
            } else {
                this.highlightHexes([], "moves");
            }
        }

        private highlightHexes(hexes: number[][], classToApply: string) {
            var hexSet: any = {};
            hexes.forEach((h) => hexSet[h.toString()]=true);
            var isHighlighted = (d: any) => hexSet[d.toString()];
            this.grid.selectAll("polygon").classed(classToApply, isHighlighted);
        }

        private drawBoard() {
            var hexes = Hex.hexagonalGrid(this.hexesOnEdge);
            var pointsFn = (d) => {
                var xy = this.qr2xy(d[0], d[1]);
                return hexPointString(this.hexSize, xy[0], xy[1]);
            }
            var update = this.grid.selectAll("polygon").data(hexes);
            update
                .enter()
                    .append("polygon")
                    .attr("points", pointsFn);

            update.exit().remove();
        }

        private hoveredHex(): number[] {
            var location = d3.mouse(this.eventLayer.node());
            var hex = this.hexFromXY(location[0], location[1]);
            return hex;
        }

        public play(g: Game, cb: (g: Game) => void): void {
            var selectedPieces: Segment;

            var disabled = false;
            var dragInProgress = false;
            var originHex = null;

            var finish = (m: Move) => {
                this.drawOverlay(null, false, g);
                disabled = true;
                cb(update(g, m));
            }

            var dragstart = () => {
                if (disabled) return;
                originHex = this.hoveredHex();
                if (selectedPieces != null) {
                    var move = generateMove(selectedPieces, originHex);
                    if (move != null && isValid(g, move)) {
                        finish(move);
                    } else {
                        selectedPieces = null;
                    }
                } else {
                    if ((selectedPieces = getSegment(g, originHex)) != null) {
                        dragInProgress = true;
                        this.drawOverlay(selectedPieces, true, g);
                    }
                }
            }

            var drag = () => {
                if (disabled) return;
                var currentHex = this.hoveredHex();
                selectedPieces = getSegment(g, originHex, currentHex);
                this.drawOverlay(selectedPieces, true, g);
            }

            var dragend = () => {
                if (disabled) return;
                var currentHex = this.hoveredHex();
                selectedPieces = getSegment(g, originHex, currentHex);
                this.drawOverlay(selectedPieces, false, g);
            }

            this.eventLayer.call(
                d3.behavior.drag()
                    .on("dragstart", dragstart)
                    .on("drag", drag)
                    .on("dragend", dragend)
            )
        }

    }

    export function moveHexes(s: Segment, g: Game): number[][] {
        return adjacentHexDirs(s)
            .filter((hd) => {
                var d = hd[1];
                var move = {segment: s, direction: d};
                return isValid(g, move);
            })
            .map((hd) => hd[0]);
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
            if (back[0][0] === undefined) debugger;
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
