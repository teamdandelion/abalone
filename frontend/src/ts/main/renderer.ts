module Main {
    export class Renderer implements PlayerAgent {
        private svg: D3.Selection;
        private board: D3.Selection;
        private whitePieces: D3.Selection;
        private blackPieces: D3.Selection;
        private whiteNumPieces: D3.Selection;
        private blackNumPieces: D3.Selection;
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
            this.whiteNumPieces = this.board.append("text")
                .attr("x", 50).attr("y", height-100)
                .classed("score-display", true).classed("white", true);
            this.blackNumPieces = this.board.append("text")
                .attr("x", 50).attr("y", 200)
                .classed("score-display", true).classed("black", true);
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

        private qr2xy(h: Abalone.Hex): [number, number] {
            return [
                this.hexSize * Math.sqrt(3) * (h.q + h.r/2) + this.width/2, 
                this.hexSize * 3/2 * h.r + this.height/2
                ];
        }

        public drawGame(g: Abalone.Game) {
            this.drawPieces(g.board);
            var whiteIsNext = g.nextPlayer === Abalone.Player.White;
            this.whitePieces.classed("faded", !whiteIsNext);
            this.blackPieces.classed("faded", whiteIsNext);
        }

        private drawPieces(b: Abalone.Board) {
            this.addPieces(this.whitePieces, b.whitePositions);
            this.addPieces(this.blackPieces, b.blackPositions);
            this.whiteNumPieces.text(b.whitePositions.length);
            this.blackNumPieces.text(b.blackPositions.length);
        }

        private addPieces(selection: D3.Selection, pieces: Abalone.Hex[]) {
            var xf = (d,i) => this.qr2xy(d)[0];
            var yf = (d,i) => this.qr2xy(d)[1];
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

        private hexFromXY(x: number, y: number): Abalone.Hex {
            x = x - this.width/2;
            y = y - this.height/2;
            var q = (x * Math.sqrt(3)/3 - y/3) / this.hexSize;
            var r = y * 2/3 / this.hexSize;
            return this.hexRound(q,r);
        }

        private hexRound(q: number, r: number): Abalone.Hex {
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
            return {q: rx, r: rz};
        }

        private drawOverlay(segment: Abalone.Segment, isDragging: boolean, game: Abalone.Game) {
            this.highlightHexes(Abalone.segPieces(segment), "selected");
            if (segment != null && !isDragging) {
                this.highlightHexes(moveHexes(segment, game), "moves");
            } else {
                this.highlightHexes([], "moves");
            }
        }

        private highlightHexes(hexes: Abalone.Hex[], classToApply: string) {
            var hexSet: any = {};
            hexes.forEach((h) => hexSet[JSON.stringify(h)]=true);
            var isHighlighted = (d: any) => hexSet[JSON.stringify(d)];
            this.grid.selectAll("polygon").classed(classToApply, isHighlighted);
        }

        private drawBoard() {
            var hexes = Abalone.hexagonalGrid(this.hexesOnEdge);
            var pointsFn = (d) => {
                var xy = this.qr2xy(d);
                return hexPointString(this.hexSize, xy[0], xy[1]);
            }
            var update = this.grid.selectAll("polygon").data(hexes);
            update
                .enter()
                    .append("polygon")
                    .attr("points", pointsFn);

            update.exit().remove();

            var xf = (d,i) => this.qr2xy(d)[0] - 5;
            var yf = (d,i) => this.qr2xy(d)[1] - 5;

            var textUpdate = this.grid.selectAll("text").data(hexes);
            textUpdate
                .enter()
                    .append("text")
                    .attr("x", xf)
                    .attr("y", yf)
                    .text((d) => "(" + d.q.toString() + "," + d.r.toString() + ")");
        }

        private hoveredHex(): Abalone.Hex {
            var location = d3.mouse(this.eventLayer.node());
            var hex = this.hexFromXY(location[0], location[1]);
            return hex;
        }

        public play(g: Abalone.Game, cb: (g: Abalone.Game) => void): void {
            var selectedPieces: Abalone.Segment;

            var disabled = false;
            var dragInProgress = false;
            var originHex = null;

            var finish = (m: Abalone.Move) => {
                this.drawOverlay(null, false, g);
                disabled = true;
                var result = Abalone.update(g, m);
                console.log("renderer/play/finish:", result);
                cb(result);
            }

            var dragstart = () => {
                if (disabled) return;
                originHex = this.hoveredHex();
                if (selectedPieces != null) {
                    var move = generateMove(selectedPieces, originHex);
                    if (move != null && Abalone.validMove(g, move)) {
                        finish(move);
                    } else {
                        selectedPieces = null;
                    }
                } else {
                    if ((selectedPieces = Abalone.getSegment(g, originHex)) != null) {
                        dragInProgress = true;
                        this.drawOverlay(selectedPieces, true, g);
                    }
                }
            }

            var drag = () => {
                if (disabled) return;
                var currentHex = this.hoveredHex();
                selectedPieces = Abalone.getSegment(g, originHex, currentHex);
                this.drawOverlay(selectedPieces, true, g);
            }

            var dragend = () => {
                if (disabled) return;
                var currentHex = this.hoveredHex();
                selectedPieces = Abalone.getSegment(g, originHex, currentHex);
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

export function moveHexes(s: Abalone.Segment, g: Abalone.Game): Abalone.Hex[] {
    return adjacentHexDirs(s)
        .filter((hd) => {
            var d = hd[1];
            var move = {segment: s, direction: d};
            return Abalone.validMove(g, move);
        })
        .map((hd) => hd[0]);
}

export function generateMove(s: Abalone.Segment, target: Abalone.Hex): Abalone.Move {
    var possibilities = adjacentHexDirs(s);
    var foundDir: Abalone.Direction;
    possibilities.forEach((hd) => {
        var hex = hd[0];
        var dir = hd[1];
        if (JSON.stringify(hex) === JSON.stringify(target)) {
            foundDir = dir;
        }
    });
    var move = foundDir != null ? {segment: s, direction: foundDir} : null;
    return move;
}

function vanguard(pos: Abalone.Hex, d: Abalone.Direction): [Abalone.Hex, Abalone.Direction][] {
    return <any> Abalone.nearbyDirections(d).map((dir) => [Abalone.adjacent(pos, dir), dir]);
}

function adjacentHexDirs(s: Abalone.Segment): [Abalone.Hex, Abalone.Direction][] {
    if (s.orientation == null) {
        return <any> Abalone.directions.map((d) => [Abalone.adjacent(s.basePos, d), d]);
    } else {
        var front = vanguard(s.basePos, Abalone.opposite(s.orientation));
        var back = vanguard(_.last(Abalone.segPieces(s)), s.orientation);
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