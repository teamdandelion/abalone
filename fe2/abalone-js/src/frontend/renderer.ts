module Abalone {
export module Frontend {
    export class Renderer {
        private svg: D3.Selection;
        private board: D3.Selection;
        private whitePieces: D3.Selection; // where white's marbles are draw
        private blackPieces: D3.Selection;
        private whiteNumPieces: D3.Selection; // display for remaining # of pieces that white controls
        private blackNumPieces: D3.Selection;
        public eventBox: D3.Selection; // box to catch drags and keypresses and proxy accordingly
                                         // public so external consumers can setup event listening on renderer
        private coordinateLayer: D3.Selection; // debug layer to show coordinates on each hex 
        private grid: D3.Selection; // background layer where hexes are drawn
        private hexesOnEdge: number; // parameter that determines number of hexes on the board
        private height: number; // height in pixels of board 
        private width: number; // width in pixels of board 
        private hexSize: number; // size of each hex in pixels (radius)
        private showDebugCoordinates = false;

        constructor(svg: any, hexesOnEdge=5) {
            this.svg = svg.node ? svg : d3.select(svg);
            this.svg.classed("abalone", true)
            this.autoGetWidthHeight();
            this.hexesOnEdge = hexesOnEdge;
            this.board = this.svg.append("g").classed("board", true);
            this.grid = this.board.append("g").classed("grid", true);
            this.whitePieces = this.board.append("g").classed("white", true);
            this.blackPieces = this.board.append("g").classed("black", true);
            this.coordinateLayer = this.board.append("g").classed("coordinate-layer", true);
            this.whiteNumPieces = this.board.append("text")
                .attr("x", 50).attr("y", this.height-100)
                .classed("score-display", true).classed("white", true);
            this.blackNumPieces = this.board.append("text")
                .attr("x", 50).attr("y", 200)
                .classed("score-display", true).classed("black", true);
            this.eventBox = this.svg.append("rect")
                                    .attr({width: this.width, height: this.height})
                                    .style({fill: "black", opacity: 0})
                                    .classed("hitbox", true);

            this.resize(this.width, this.height);
        }

        private autoGetWidthHeight() {
            // this fn taken from Plottable (component.ts). Almost certainly more 
            // complex than is needed since Plottable cares about browser compat
            // and we don't, but why not
            if (this.svg.attr("width") == null) {
              this.svg.attr("width", "100%");
            }
            if (this.svg.attr("height") == null) {
              this.svg.attr("height", "100%");
            }

            function _getParsedStyleValue(style: CSSStyleDeclaration, prop: string): number {
              var value: any = style.getPropertyValue(prop);
              if (value == null){
                return 0;
              }
              return parseFloat(value);
            }

            function getElementWidth(elem: HTMLScriptElement): number{
              var style: CSSStyleDeclaration = window.getComputedStyle(elem);
              return _getParsedStyleValue(style, "width")
                + _getParsedStyleValue(style, "padding-left")
                + _getParsedStyleValue(style, "padding-right")
                + _getParsedStyleValue(style, "border-left-width")
                + _getParsedStyleValue(style, "border-right-width");
            }

            function getElementHeight(elem: HTMLScriptElement): number{
              var style: CSSStyleDeclaration = window.getComputedStyle(elem);
              return _getParsedStyleValue(style, "height")
                + _getParsedStyleValue(style, "padding-top")
                + _getParsedStyleValue(style, "padding-bottom")
                + _getParsedStyleValue(style, "border-top-width")
                + _getParsedStyleValue(style, "border-bottom-width");
            }

            var elem: HTMLScriptElement = (<HTMLScriptElement> this.svg.node());
            this.width  = getElementWidth(elem);
            this.height = getElementHeight(elem);
        }

        public resize(width, height) {
            this.width = width;
            this.height = height;
            this.hexSize = Math.min(width, height) / this.hexesOnEdge / 4;
            this.drawBoard();
        }

        private qr2xy(h: Engine.Hex): [number, number] {
            return [
                this.hexSize * Math.sqrt(3) * (h.q + h.r/2) + this.width/2, 
                this.hexSize * 3/2 * h.r + this.height/2
                ];
        }

        public drawGame(g: Engine.Game) {
            this.drawPieces(g.board);
            var whiteIsNext = g.nextPlayer === Engine.Player.White;
            this.whitePieces.classed("faded", !whiteIsNext);
            this.blackPieces.classed("faded", whiteIsNext);
        }

        private drawPieces(b: Engine.Board) {
            this.addPieces(this.whitePieces, b.whitePositions);
            this.addPieces(this.blackPieces, b.blackPositions);
            this.whiteNumPieces.text(b.whitePositions.length);
            this.blackNumPieces.text(b.blackPositions.length);
        }

        private addPieces(selection: D3.Selection, pieces: Engine.Hex[]) {
            var xf = (d,i) => this.qr2xy(d)[0];
            var yf = (d,i) => this.qr2xy(d)[1];
            var update = selection
                .selectAll("circle")
                .data(pieces, (kp) => kp.id);
            update
                .enter()
                    .append("circle")
                        .attr("r",  this.hexSize/2)
                        .attr("cx", xf)
                        .attr("cy", yf);
            update.transition()
                .attr("cx", xf)
                .attr("cy", yf)
            update
                .exit().remove();

        }

        private hexFromXY(x: number, y: number): Engine.Hex {
            x = x - this.width/2;
            y = y - this.height/2;
            var q = (x * Math.sqrt(3)/3 - y/3) / this.hexSize;
            var r = y * 2/3 / this.hexSize;
            return this.hexRound(q,r);
        }

        private hexRound(q: number, r: number): Engine.Hex {
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

        public drawOverlay(segment: Engine.Segment, isDragging: boolean, game: Engine.Game) {
            this.highlightHexes(Engine.segPieces(segment), "selected");
            if (segment != null && !isDragging) {
                this.highlightHexes(moveHexes(segment, game), "moves");
            } else {1
                this.highlightHexes([], "moves");
            }
        }

        private highlightHexes(hexes: Engine.Hex[], classToApply: string) {
            var hexSet: any = {};
            hexes.forEach((h) => hexSet[JSON.stringify(h)]=true);
            var isHighlighted = (d: any) => hexSet[JSON.stringify(d)];
            this.grid.selectAll("polygon").classed(classToApply, isHighlighted);
        }

        private drawBoard() {
            var hexes = Engine.hexagonalGrid(this.hexesOnEdge);
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

            if (this.showDebugCoordinates) {
                var textUpdate = this.coordinateLayer.selectAll("text").data(hexes);
                textUpdate
                    .enter()
                        .append("text")
                        .attr("x", xf)
                        .attr("y", yf)
                        .text((d) => "(" + d.q.toString() + "," + d.r.toString() + ")");
                
            }
        }

        public hoveredHex(): Engine.Hex {
            var location = d3.mouse(this.eventBox.node());
            var hex = this.hexFromXY(location[0], location[1]);
            return hex;
        }
    }

export function moveHexes(s: Engine.Segment, g: Engine.Game): Engine.Hex[] {
    return adjacentHexDirs(s)
        .filter((hd) => {
            var d = hd[1];
            var move = {segment: s, direction: d};
            return Engine.validMove(g, move);
        })
        .map((hd) => hd[0]);
}

export function generateMove(s: Engine.Segment, target: Engine.Hex): Engine.Move {
    var possibilities = adjacentHexDirs(s);
    var foundDir: Engine.Direction;
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

function vanguard(pos: Engine.Hex, d: Engine.Direction): [Engine.Hex, Engine.Direction][] {
    return <any> Engine.nearbyDirections(d).map((dir) => [Engine.adjacent(pos, dir), dir]);
}

function adjacentHexDirs(s: Engine.Segment): [Engine.Hex, Engine.Direction][] {
    if (s.orientation == null) {
        return <any> Engine.directions.map((d) => [Engine.adjacent(s.basePos, d), d]);
    } else {
        var front = vanguard(s.basePos, Engine.opposite(s.orientation));
        var pieces = Engine.segPieces(s);
        var lastPiece = pieces[pieces.length-1];
        var back = vanguard(lastPiece, s.orientation);
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
}