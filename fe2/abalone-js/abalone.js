var Abalone;
(function (Abalone) {
    var Engine;
    (function (Engine) {
        function hexIndex(hexes, x) {
            for (var i = 0; i < hexes.length; i++) {
                if (hexes[i].q === x.q && hexes[i].r === x.r)
                    return i;
            }
            return -1;
        }
        Engine.hexIndex = hexIndex;
        function hexagonalGrid(hexesOnEdge) {
            var out = [];
            for (var r = 0; r < hexesOnEdge; r++) {
                out = out.concat(ring(r));
            }
            return out;
        }
        Engine.hexagonalGrid = hexagonalGrid;
        function hexstr(h) {
            return "<" + h.q.toString() + "," + h.r.toString() + ">";
        }
        Engine.hexstr = hexstr;
        function ring(rad) {
            var current = { q: -rad, r: 0 };
            if (rad === 0)
                return [current];
            var out = [];
            Engine.directions.forEach(function (d) {
                for (var r = 0; r < rad; r++) {
                    current = adjacent(current, d);
                    out.push(current);
                }
            });
            return out;
        }
        Engine.ring = ring;
        function nearbyDirections(d) {
            var idx = Engine.directions.indexOf(d);
            var nearby = [-1, 0, 1].map(function (x) { return (x + idx + 6) % 6; });
            return nearby.map(function (i) { return Engine.directions[i]; });
        }
        Engine.nearbyDirections = nearbyDirections;
        function findDirection(p1, p2) {
            var q1 = p1.q;
            var r1 = p1.r;
            var q2 = p2.q;
            var r2 = p2.r;
            if (q1 === q2 && r1 === r2) {
                return null;
            }
            else if (q1 === q2) {
                return r1 < r2 ? 2 /* BotRight */ : 3 /* TopLeft */;
            }
            else if (r1 === r2) {
                return q1 < q2 ? 1 /* MidRight */ : 4 /* MidLeft */;
            }
            else if (r1 + q1 === r2 + q2) {
                return q1 < q2 ? 0 /* TopRight */ : 5 /* BotLeft */;
            }
            else {
                return null;
            }
        }
        Engine.findDirection = findDirection;
        function dist2(x1, x2) {
            return Math.abs(x1.q - x2.q) + Math.abs(x1.r - x2.r) + Math.abs(x1.q + x1.r - x2.q - x2.r);
        }
        Engine.dist2 = dist2;
        function dist(x1, x2) {
            return Math.round(dist2(x1, x2) / 2);
        }
        Engine.dist = dist;
        function onBoard(b, p) {
            return dist2(p, { q: 0, r: 0 }) < b.edgeLength * 2;
        }
        Engine.onBoard = onBoard;
        function opposite(d) {
            switch (d) {
                case 0 /* TopRight */: return 5 /* BotLeft */;
                case 1 /* MidRight */: return 4 /* MidLeft */;
                case 2 /* BotRight */: return 3 /* TopLeft */;
                case 5 /* BotLeft */: return 0 /* TopRight */;
                case 4 /* MidLeft */: return 1 /* MidRight */;
                case 3 /* TopLeft */: return 2 /* BotRight */;
            }
        }
        Engine.opposite = opposite;
        function colinear(d1, d2) {
            return d1 === d2 || d1 === opposite(d2);
        }
        Engine.colinear = colinear;
        function adjacent(position, d) {
            var q = position.q;
            var r = position.r;
            switch (d) {
                case 0 /* TopRight */: return { q: q + 1, r: r - 1 };
                case 1 /* MidRight */: return { q: q + 1, r: r };
                case 2 /* BotRight */: return { q: q, r: r + 1 };
                case 5 /* BotLeft */: return { q: q - 1, r: r + 1 };
                case 4 /* MidLeft */: return { q: q - 1, r: r };
                case 3 /* TopLeft */: return { q: q, r: r - 1 };
            }
        }
        Engine.adjacent = adjacent;
    })(Engine = Abalone.Engine || (Abalone.Engine = {}));
})(Abalone || (Abalone = {}));
var Abalone;
(function (Abalone) {
    var Engine;
    (function (Engine) {
        (function (Player) {
            Player[Player["White"] = 0] = "White";
            Player[Player["Black"] = 1] = "Black";
        })(Engine.Player || (Engine.Player = {}));
        var Player = Engine.Player;
        function next(p) {
            return (p === 0 /* White */) ? 1 /* Black */ : 0 /* White */;
        }
        Engine.next = next;
        function player2str(p) {
            return p === 0 /* White */ ? "white" : "black";
        }
        Engine.player2str = player2str;
        function str2player(s) {
            switch (s) {
                case "white":
                    return 0 /* White */;
                case "black":
                    return 1 /* Black */;
                default:
                    return null;
            }
        }
        Engine.str2player = str2player;
    })(Engine = Abalone.Engine || (Abalone.Engine = {}));
})(Abalone || (Abalone = {}));
var Abalone;
(function (Abalone) {
    var Engine;
    (function (Engine) {
        function free(b, x) {
            return owner(b, x) == null;
        }
        Engine.free = free;
        function owner(b, x) {
            if (Engine.hexIndex(b.whitePositions, x) !== -1)
                return 0 /* White */;
            if (Engine.hexIndex(b.blackPositions, x) !== -1)
                return 1 /* Black */;
            return null;
        }
        Engine.owner = owner;
        function getPieces(b, p) {
            if (p === 0 /* White */) {
                return b.whitePositions;
            }
            else {
                return b.blackPositions;
            }
        }
        Engine.getPieces = getPieces;
        function tuplesToHexes(tups) {
            return tups.map(function (t) {
                return { q: t[0], r: t[1] };
            });
        }
        Engine.tuplesToHexes = tuplesToHexes;
        function standardBoard() {
            return {
                edgeLength: 5,
                whitePositions: tuplesToHexes([
                    [-4, 3],
                    [-4, 4],
                    [-3, 3],
                    [-3, 4],
                    [-2, 2],
                    [-2, 3],
                    [-2, 4],
                    [-1, 2],
                    [-1, 3],
                    [-1, 4],
                    [0, 2],
                    [0, 3],
                    [0, 4],
                    [1, 3]
                ]),
                blackPositions: tuplesToHexes([
                    [-1, -3],
                    [0, -4],
                    [0, -3],
                    [0, -2],
                    [1, -4],
                    [1, -3],
                    [1, -2],
                    [2, -4],
                    [2, -3],
                    [2, -2],
                    [3, -4],
                    [3, -3],
                    [4, -4],
                    [4, -3]
                ])
            };
        }
        Engine.standardBoard = standardBoard;
        function smallBoard() {
            return {
                edgeLength: 2,
                whitePositions: tuplesToHexes([
                    [-1, 1],
                    [0, 1],
                    [0, 0]
                ]),
                blackPositions: tuplesToHexes([
                    [0, -1],
                    [1, -1]
                ])
            };
        }
        Engine.smallBoard = smallBoard;
    })(Engine = Abalone.Engine || (Abalone.Engine = {}));
})(Abalone || (Abalone = {}));
var Abalone;
(function (Abalone) {
    var Engine;
    (function (Engine) {
        (function (Direction) {
            Direction[Direction["TopRight"] = 0] = "TopRight";
            Direction[Direction["MidRight"] = 1] = "MidRight";
            Direction[Direction["BotRight"] = 2] = "BotRight";
            Direction[Direction["TopLeft"] = 3] = "TopLeft";
            Direction[Direction["MidLeft"] = 4] = "MidLeft";
            Direction[Direction["BotLeft"] = 5] = "BotLeft";
        })(Engine.Direction || (Engine.Direction = {}));
        var Direction = Engine.Direction;
        Engine.directions = [0 /* TopRight */, 1 /* MidRight */, 2 /* BotRight */, 5 /* BotLeft */, 4 /* MidLeft */, 3 /* TopLeft */];
    })(Engine = Abalone.Engine || (Abalone.Engine = {}));
})(Abalone || (Abalone = {}));
var Abalone;
(function (Abalone) {
    var Engine;
    (function (Engine) {
        (function (Outcome) {
            Outcome[Outcome["WhiteWins"] = 0] = "WhiteWins";
            Outcome[Outcome["BlackWins"] = 1] = "BlackWins";
            Outcome[Outcome["TieGame"] = 2] = "TieGame";
        })(Engine.Outcome || (Engine.Outcome = {}));
        var Outcome = Engine.Outcome;
    })(Engine = Abalone.Engine || (Abalone.Engine = {}));
})(Abalone || (Abalone = {}));
var Abalone;
(function (Abalone) {
    var Engine;
    (function (Engine) {
        function segPieces(s) {
            if (s == null)
                return [];
            var front = s.basePos;
            var pieces = [front];
            for (var i = 0; i < s.segLength - 1; i++) {
                front = Engine.adjacent(front, s.orientation);
                pieces.push(front);
            }
            return pieces;
        }
        Engine.segPieces = segPieces;
    })(Engine = Abalone.Engine || (Abalone.Engine = {}));
})(Abalone || (Abalone = {}));
var Abalone;
(function (Abalone) {
    var Engine;
    (function (Engine) {
        function validMove(g, m) {
            if (broadside(m)) {
                return Engine.segPieces(m.segment).map(function (p) { return Engine.adjacent(p, m.direction); }).every(function (p) { return Engine.free(g.board, p); });
            }
            else {
                return inlineMoved(g.board, m) !== null;
            }
        }
        Engine.validMove = validMove;
        function inline(m) {
            return m.segment.orientation !== null && Engine.colinear(m.direction, m.segment.orientation);
        }
        function broadside(m) {
            return !inline(m);
        }
        Engine.broadside = broadside;
        function inlineMoved(b, m) {
            if (broadside(m))
                return null;
            var pieces = Engine.segPieces(m.segment);
            var attacked = m.segment.orientation === m.direction ? pieces[pieces.length - 1] : pieces[0];
            var movedEnemyPieces = [];
            for (var i = 0; i < m.segment.segLength; i++) {
                attacked = Engine.adjacent(attacked, m.direction);
                var controller = Engine.owner(b, attacked);
                if (controller == null)
                    return movedEnemyPieces;
                if (controller === m.segment.player)
                    return null;
                movedEnemyPieces.push(attacked);
            }
            return null;
        }
        Engine.inlineMoved = inlineMoved;
    })(Engine = Abalone.Engine || (Abalone.Engine = {}));
})(Abalone || (Abalone = {}));
var Abalone;
(function (Abalone) {
    var Engine;
    (function (Engine) {
        function initializeIDs(game) {
            var i = 0;
            game.board.whitePositions.forEach(function (h) {
                h.id = i++;
            });
            game.board.blackPositions.forEach(function (h) {
                h.id = i++;
            });
        }
        Engine.initializeIDs = initializeIDs;
        function deduceIDs(currentState, lastState) {
            var idMap = {};
            var getID = function (h) { return idMap[Engine.hexstr(h)] = h.id; };
            lastState.board.whitePositions.forEach(getID);
            lastState.board.blackPositions.forEach(getID);
            var generatingMove = findGeneratingMove(lastState, currentState);
            function getPieceMovements(g, m) {
                var ownPieces = Engine.segPieces(m.segment);
                var enemyPieces = Engine.broadside(m) ? [] : Engine.inlineMoved(g.board, m);
                return ownPieces.concat(enemyPieces);
            }
            var hexesToMove = getPieceMovements(lastState, generatingMove);
            var staging = {};
            hexesToMove.forEach(function (h) {
                var k = Engine.hexstr(h);
                staging[k] = idMap[k];
                delete idMap[k];
            });
            hexesToMove.forEach(function (h) {
                var originalK = Engine.hexstr(h);
                var movedK = Engine.hexstr(Engine.adjacent(h, generatingMove.direction));
                idMap[movedK] = staging[originalK];
            });
            var addID = function (h) { return h.id = idMap[Engine.hexstr(h)]; };
            currentState.board.whitePositions.forEach(addID);
            currentState.board.blackPositions.forEach(addID);
        }
        Engine.deduceIDs = deduceIDs;
        function removeIDs(game) {
            game.board.whitePositions.forEach(function (h) {
                delete h.id;
            });
            game.board.blackPositions.forEach(function (h) {
                delete h.id;
            });
        }
        function standardGame() {
            return {
                lossThreshold: 8,
                marblesPerMove: 3,
                movesRemaining: 200,
                nextPlayer: 0 /* White */,
                board: Engine.standardBoard()
            };
        }
        Engine.standardGame = standardGame;
        function smallGame() {
            return {
                lossThreshold: 1,
                marblesPerMove: 3,
                movesRemaining: 200,
                nextPlayer: 0 /* White */,
                board: Engine.smallBoard()
            };
        }
        Engine.smallGame = smallGame;
        function findGeneratingMove(initial, future) {
            var ms = moves(initial);
            for (var i = 0; i < ms.length; i++) {
                var f = update(initial, ms[i]);
                if (gameEq(f, future)) {
                    return ms[i];
                }
            }
            return null;
        }
        Engine.findGeneratingMove = findGeneratingMove;
        function validFuture(initial, future) {
            var theFutures = futures(initial);
            for (var i = 0; i < theFutures.length; i++) {
                if (gameEq(future, theFutures[i])) {
                    return true;
                }
            }
            return false;
        }
        Engine.validFuture = validFuture;
        function gameEq(g1, g2) {
            function boardEq(b1, b2) {
                return b1.edgeLength === b2.edgeLength && arrayIsPermutation(b1.whitePositions, b2.whitePositions) && arrayIsPermutation(b1.blackPositions, b2.blackPositions);
            }
            function arrayIsPermutation(a1, a2) {
                if (a1.length !== a2.length)
                    return false;
                var present = {};
                var i;
                var k;
                for (i = 0; i < a1.length; i++) {
                    k = Engine.hexstr(a1[i]);
                    present[k] = true;
                }
                for (i = 0; i < a2.length; i++) {
                    k = Engine.hexstr(a2[i]);
                    if (!present[k]) {
                        return false;
                    }
                }
                return true;
            }
            return g1.nextPlayer === g2.nextPlayer && g1.movesRemaining === g2.movesRemaining && g1.lossThreshold === g2.lossThreshold && g1.marblesPerMove === g2.marblesPerMove && boardEq(g1.board, g2.board);
        }
        Engine.gameEq = gameEq;
        function copyJSON(a) {
            return JSON.parse(JSON.stringify(a));
        }
        function serializeGame(g) {
            var copiedState = copyJSON(g);
            removeIDs(copiedState);
            copiedState.nextPlayer = Engine.player2str(g.nextPlayer);
            return JSON.stringify(copiedState);
        }
        Engine.serializeGame = serializeGame;
        function parseJSON(s) {
            s.nextPlayer = Engine.str2player(s.nextPlayer);
            return s;
        }
        Engine.parseJSON = parseJSON;
        function deserializeGame(s) {
            var g = JSON.parse(s);
            if (g.state != null) {
                g = g.state;
            }
            g.nextPlayer = Engine.str2player(g.nextPlayer);
            return g;
        }
        Engine.deserializeGame = deserializeGame;
        function winner(g) {
            if (gameOver(g)) {
                var w = g.board.whitePositions.length;
                var b = g.board.blackPositions.length;
                if (w > b)
                    return 0 /* WhiteWins */;
                if (b > w)
                    return 1 /* BlackWins */;
                return 2 /* TieGame */;
            }
            return null;
        }
        Engine.winner = winner;
        function moves(g) {
            var allMoves = [];
            segments(g).forEach(function (s) {
                Engine.directions.forEach(function (d) {
                    allMoves.push({ segment: s, direction: d });
                });
            });
            return allMoves.filter(function (m) { return Engine.validMove(g, m); });
        }
        Engine.moves = moves;
        function gameOver(g) {
            var b = g.board;
            return g.movesRemaining <= 0 || Math.min(b.whitePositions.length, b.blackPositions.length) <= g.lossThreshold;
        }
        Engine.gameOver = gameOver;
        function segments(g) {
            var pieces = Engine.getPieces(g.board, g.nextPlayer);
            var presentSet = {};
            var singletons = pieces.map(function (pos) {
                presentSet[Engine.hexstr(pos)] = true;
                return { basePos: pos, orientation: null, segLength: 1, player: g.nextPlayer };
            });
            var twoOrMore = [];
            pieces.forEach(function (pos) {
                [0 /* TopRight */, 1 /* MidRight */, 2 /* BotRight */].forEach(function (d) {
                    var nextPiece = Engine.adjacent(pos, d);
                    var length = 2;
                    while (length <= g.marblesPerMove && presentSet[Engine.hexstr(nextPiece)]) {
                        twoOrMore.push({
                            basePos: pos,
                            orientation: d,
                            segLength: length,
                            player: g.nextPlayer
                        });
                        nextPiece = Engine.adjacent(nextPiece, d);
                        length++;
                    }
                });
            });
            return singletons.concat(twoOrMore);
        }
        Engine.segments = segments;
        function getSegment(g, origin, destination) {
            function getProposedSegment(origin, destination) {
                if (destination == null || Engine.hexstr(origin) === Engine.hexstr(destination)) {
                    return { basePos: origin, segLength: 1, player: g.nextPlayer, orientation: null };
                }
                var d = Engine.findDirection(origin, destination);
                if (d != null) {
                    return {
                        basePos: origin,
                        segLength: Engine.dist(origin, destination) + 1,
                        orientation: d,
                        player: g.nextPlayer
                    };
                }
            }
            var proposedSegment = getProposedSegment(origin, destination);
            var pieces = Engine.getPieces(g.board, g.nextPlayer);
            var pieceSet = {};
            pieces.forEach(function (p) { return pieceSet[Engine.hexstr(p)] = true; });
            var pieceChecker = function (p) {
                return pieceSet[Engine.hexstr(p)];
            };
            if (proposedSegment && Engine.segPieces(proposedSegment).every(pieceChecker)) {
                return proposedSegment;
            }
            else {
                return null;
            }
        }
        Engine.getSegment = getSegment;
        function update(g, m) {
            var ownPieces = Engine.segPieces(m.segment);
            var enemyPieces = Engine.broadside(m) ? [] : Engine.inlineMoved(g.board, m);
            var whiteMoved = (g.nextPlayer === 0 /* White */) ? ownPieces : enemyPieces;
            var blackMoved = (g.nextPlayer === 0 /* White */) ? enemyPieces : ownPieces;
            var movePieces = function (ps) {
                return ps.map(function (p) { return Engine.adjacent(p, m.direction); }).filter(function (p) { return Engine.onBoard(g.board, p); });
            };
            function removeAll(source, remove) {
                var out = source.slice();
                remove.forEach(function (x) {
                    var idx = Engine.hexIndex(out, x);
                    if (idx !== -1) {
                        out.splice(idx, 1);
                    }
                });
                return out;
            }
            var newWhite = removeAll(g.board.whitePositions, whiteMoved).concat(movePieces(whiteMoved));
            var newBlack = removeAll(g.board.blackPositions, blackMoved).concat(movePieces(blackMoved));
            var newBoard = {
                whitePositions: newWhite,
                blackPositions: newBlack,
                edgeLength: g.board.edgeLength
            };
            return {
                board: newBoard,
                nextPlayer: Engine.next(g.nextPlayer),
                movesRemaining: g.movesRemaining - 1,
                marblesPerMove: g.marblesPerMove,
                lossThreshold: g.lossThreshold
            };
        }
        Engine.update = update;
        function futures(g) {
            return moves(g).map(function (m) { return update(g, m); });
        }
        Engine.futures = futures;
    })(Engine = Abalone.Engine || (Abalone.Engine = {}));
})(Abalone || (Abalone = {}));
var Abalone;
(function (Abalone) {
    var Frontend;
    (function (Frontend) {
        var GameReplayer = (function () {
            function GameReplayer() {
                this.delay = 1000;
                this.playing = false;
                this.idx = 0;
            }
            GameReplayer.prototype.setRenderer = function (r) {
                this.renderer = r;
            };
            GameReplayer.prototype.setHistory = function (history) {
                this.history = history;
                if (this.history.length > 0) {
                    Abalone.Engine.initializeIDs(this.history[0]);
                    for (var i = 1; i < this.history.length; i++) {
                        Abalone.Engine.deduceIDs(this.history[i], this.history[i - 1]);
                    }
                }
            };
            GameReplayer.prototype.draw = function () {
                var state = this.history[this.idx];
                this.renderer.drawGame(state);
            };
            GameReplayer.prototype.back = function () {
                this.playing = false;
                if (this.idx === 0)
                    return;
                this.idx--;
                this.draw();
            };
            GameReplayer.prototype.forward = function () {
                this.playing = false;
                if (this.idx === this.history.length - 1)
                    return;
                this.idx++;
                this.draw();
            };
            GameReplayer.prototype.pause = function () {
                this.playing = false;
            };
            GameReplayer.prototype.play = function () {
                if (!this.playing) {
                    this.playing = true;
                    this.replay();
                }
            };
            GameReplayer.prototype.restart = function () {
                this.playing = false;
                this.idx = 0;
                this.draw();
            };
            GameReplayer.prototype.skipToEnd = function () {
                this.playing = false;
                this.idx = this.history.length - 1;
                this.draw();
            };
            GameReplayer.prototype.speedUp = function () {
                this.delay /= 2;
            };
            GameReplayer.prototype.slowDown = function () {
                this.delay *= 2;
            };
            GameReplayer.prototype.replay = function () {
                if (this.idx === this.history.length - 1) {
                    this.playing = false;
                }
                if (this.playing) {
                    this.draw();
                    this.idx++;
                    setTimeout(this.replay.bind(this), this.delay);
                }
            };
            return GameReplayer;
        })();
        Frontend.GameReplayer = GameReplayer;
    })(Frontend = Abalone.Frontend || (Abalone.Frontend = {}));
})(Abalone || (Abalone = {}));
var Abalone;
(function (Abalone) {
    var Frontend;
    (function (Frontend) {
        var port = "1337";
        var InteractiveGame = (function () {
            function InteractiveGame(renderer, white, black) {
                this.renderer = renderer;
                this.white = white;
                this.black = black;
            }
            InteractiveGame.prototype.start = function (game) {
                this.gameLoop(game);
            };
            InteractiveGame.prototype.gameLoop = function (game) {
                if (this.lastState != null && !Abalone.Engine.validFuture(this.lastState, game)) {
                    debugger;
                }
                if (this.lastState == null) {
                    Abalone.Engine.initializeIDs(game);
                }
                else {
                    Abalone.Engine.deduceIDs(game, this.lastState);
                }
                this.lastState = game;
                this.renderer.drawGame(game);
                if (Abalone.Engine.gameOver(game)) {
                    return;
                }
                var agent = game.nextPlayer === 0 /* White */ ? this.white : this.black;
                agent.play(game, this.gameLoop.bind(this));
            };
            return InteractiveGame;
        })();
        Frontend.InteractiveGame = InteractiveGame;
    })(Frontend = Abalone.Frontend || (Abalone.Frontend = {}));
})(Abalone || (Abalone = {}));
var Abalone;
(function (Abalone) {
    var Frontend;
    (function (Frontend) {
        var LocalPlayer = (function () {
            function LocalPlayer(renderer) {
                this.renderer = renderer;
            }
            LocalPlayer.prototype.play = function (g, cb) {
                var _this = this;
                var selectedPieces;
                var disabled = false;
                var dragInProgress = false;
                var originHex = null;
                var finish = function (m) {
                    _this.renderer.drawOverlay(null, false, g);
                    disabled = true;
                    var result = Abalone.Engine.update(g, m);
                    console.log("renderer/play/finish:", result);
                    cb(result);
                };
                var dragstart = function () {
                    if (disabled)
                        return;
                    originHex = _this.renderer.hoveredHex();
                    if (selectedPieces != null) {
                        var move = Frontend.generateMove(selectedPieces, originHex);
                        if (move != null && Abalone.Engine.validMove(g, move)) {
                            finish(move);
                        }
                        else {
                            selectedPieces = null;
                        }
                    }
                    else {
                        if ((selectedPieces = Abalone.Engine.getSegment(g, originHex)) != null) {
                            dragInProgress = true;
                            _this.renderer.drawOverlay(selectedPieces, true, g);
                        }
                    }
                };
                var drag = function () {
                    if (disabled)
                        return;
                    var currentHex = _this.renderer.hoveredHex();
                    selectedPieces = Abalone.Engine.getSegment(g, originHex, currentHex);
                    _this.renderer.drawOverlay(selectedPieces, true, g);
                };
                var dragend = function () {
                    if (disabled)
                        return;
                    var currentHex = _this.renderer.hoveredHex();
                    selectedPieces = Abalone.Engine.getSegment(g, originHex, currentHex);
                    _this.renderer.drawOverlay(selectedPieces, false, g);
                };
                this.renderer.eventBox.call(d3.behavior.drag().on("dragstart", dragstart).on("drag", drag).on("dragend", dragend));
            };
            return LocalPlayer;
        })();
        Frontend.LocalPlayer = LocalPlayer;
    })(Frontend = Abalone.Frontend || (Abalone.Frontend = {}));
})(Abalone || (Abalone = {}));
var Abalone;
(function (Abalone) {
    var Frontend;
    (function (Frontend) {
        var RemotePlayer = (function () {
            function RemotePlayer(port) {
                this.port = port;
            }
            RemotePlayer.prototype.play = function (g, cb) {
                var _this = this;
                console.log("RemotePlayer - Play:", g);
                var gameToSend = Abalone.Engine.serializeGame(g);
                var xhr = d3.xhr("http://localhost:" + this.port + "/frontend", "application/json");
                xhr.post(gameToSend, function (err, resp) {
                    var xhr = d3.xhr("http://localhost:" + _this.port + "/frontend", "application/json");
                    if (err)
                        console.log("remote player - outer:", err);
                    xhr.get(function (err, resp) {
                        console.log("RemotePlayer - Rec game back:", resp.response);
                        if (err)
                            console.log("remote player - inner:", err);
                        var responseGame = Abalone.Engine.deserializeGame(resp.response);
                        cb(responseGame);
                    });
                });
            };
            return RemotePlayer;
        })();
        Frontend.RemotePlayer = RemotePlayer;
    })(Frontend = Abalone.Frontend || (Abalone.Frontend = {}));
})(Abalone || (Abalone = {}));
var Abalone;
(function (Abalone) {
    var Frontend;
    (function (Frontend) {
        var Renderer = (function () {
            function Renderer(svg, hexesOnEdge) {
                if (hexesOnEdge === void 0) { hexesOnEdge = 5; }
                this.svg = svg.node ? svg : d3.select(svg);
                this.svg.classed("abalone", true);
                this.autoGetWidthHeight();
                this.hexesOnEdge = hexesOnEdge;
                this.board = this.svg.append("g").classed("board", true);
                this.grid = this.board.append("g").classed("grid", true);
                this.whitePieces = this.board.append("g").classed("white", true);
                this.blackPieces = this.board.append("g").classed("black", true);
                this.coordinateLayer = this.board.append("g").classed("coordinate-layer", true);
                this.whiteNumPieces = this.board.append("text").attr("x", 50).attr("y", this.height - 100).classed("score-display", true).classed("white", true);
                this.blackNumPieces = this.board.append("text").attr("x", 50).attr("y", 200).classed("score-display", true).classed("black", true);
                this.eventBox = this.svg.append("rect").attr({ width: this.width, height: this.height }).style({ fill: "black", opacity: 0 }).classed("hitbox", true);
                this.resize(this.width, this.height);
            }
            Renderer.prototype.autoGetWidthHeight = function () {
                if (this.svg.attr("width") == null) {
                    this.svg.attr("width", "100%");
                }
                if (this.svg.attr("height") == null) {
                    this.svg.attr("height", "100%");
                }
                function _getParsedStyleValue(style, prop) {
                    var value = style.getPropertyValue(prop);
                    if (value == null) {
                        return 0;
                    }
                    return parseFloat(value);
                }
                function getElementWidth(elem) {
                    var style = window.getComputedStyle(elem);
                    return _getParsedStyleValue(style, "width") + _getParsedStyleValue(style, "padding-left") + _getParsedStyleValue(style, "padding-right") + _getParsedStyleValue(style, "border-left-width") + _getParsedStyleValue(style, "border-right-width");
                }
                function getElementHeight(elem) {
                    var style = window.getComputedStyle(elem);
                    return _getParsedStyleValue(style, "height") + _getParsedStyleValue(style, "padding-top") + _getParsedStyleValue(style, "padding-bottom") + _getParsedStyleValue(style, "border-top-width") + _getParsedStyleValue(style, "border-bottom-width");
                }
                var elem = this.svg.node();
                this.width = getElementWidth(elem);
                this.height = getElementHeight(elem);
            };
            Renderer.prototype.resize = function (width, height) {
                this.width = width;
                this.height = height;
                this.hexSize = Math.min(width, height) / this.hexesOnEdge / 4;
                this.drawBoard();
            };
            Renderer.prototype.qr2xy = function (h) {
                return [
                    this.hexSize * Math.sqrt(3) * (h.q + h.r / 2) + this.width / 2,
                    this.hexSize * 3 / 2 * h.r + this.height / 2
                ];
            };
            Renderer.prototype.drawGame = function (g) {
                this.drawPieces(g.board);
                var whiteIsNext = g.nextPlayer === 0 /* White */;
                this.whitePieces.classed("faded", !whiteIsNext);
                this.blackPieces.classed("faded", whiteIsNext);
            };
            Renderer.prototype.drawPieces = function (b) {
                this.addPieces(this.whitePieces, b.whitePositions);
                this.addPieces(this.blackPieces, b.blackPositions);
                this.whiteNumPieces.text(b.whitePositions.length);
                this.blackNumPieces.text(b.blackPositions.length);
            };
            Renderer.prototype.addPieces = function (selection, pieces) {
                var _this = this;
                var xf = function (d, i) { return _this.qr2xy(d)[0]; };
                var yf = function (d, i) { return _this.qr2xy(d)[1]; };
                var update = selection.selectAll("circle").data(pieces, function (kp) { return kp.id; });
                update.enter().append("circle").attr("r", this.hexSize / 2).attr("cx", xf).attr("cy", yf);
                update.transition().attr("cx", xf).attr("cy", yf);
                update.exit().remove();
            };
            Renderer.prototype.hexFromXY = function (x, y) {
                x = x - this.width / 2;
                y = y - this.height / 2;
                var q = (x * Math.sqrt(3) / 3 - y / 3) / this.hexSize;
                var r = y * 2 / 3 / this.hexSize;
                return this.hexRound(q, r);
            };
            Renderer.prototype.hexRound = function (q, r) {
                var x = q;
                var z = r;
                var y = -x - z;
                var rx = Math.round(x);
                var ry = Math.round(y);
                var rz = Math.round(z);
                var xdiff = Math.abs(rx - x);
                var ydiff = Math.abs(ry - y);
                var zdiff = Math.abs(rz - z);
                if (xdiff > ydiff && xdiff > zdiff) {
                    rx = -ry - rz;
                }
                else if (ydiff > zdiff) {
                    ry = -rx - rz;
                }
                else {
                    rz = -rx - ry;
                }
                return { q: rx, r: rz };
            };
            Renderer.prototype.drawOverlay = function (segment, isDragging, game) {
                this.highlightHexes(Abalone.Engine.segPieces(segment), "selected");
                if (segment != null && !isDragging) {
                    this.highlightHexes(moveHexes(segment, game), "moves");
                }
                else {
                    1;
                    this.highlightHexes([], "moves");
                }
            };
            Renderer.prototype.highlightHexes = function (hexes, classToApply) {
                var hexSet = {};
                hexes.forEach(function (h) { return hexSet[JSON.stringify(h)] = true; });
                var isHighlighted = function (d) { return hexSet[JSON.stringify(d)]; };
                this.grid.selectAll("polygon").classed(classToApply, isHighlighted);
            };
            Renderer.prototype.drawBoard = function () {
                var _this = this;
                var hexes = Abalone.Engine.hexagonalGrid(this.hexesOnEdge);
                var pointsFn = function (d) {
                    var xy = _this.qr2xy(d);
                    return hexPointString(_this.hexSize, xy[0], xy[1]);
                };
                var update = this.grid.selectAll("polygon").data(hexes);
                update.enter().append("polygon").attr("points", pointsFn);
                update.exit().remove();
                var xf = function (d, i) { return _this.qr2xy(d)[0] - 5; };
                var yf = function (d, i) { return _this.qr2xy(d)[1] - 5; };
                var textUpdate = this.coordinateLayer.selectAll("text").data(hexes);
                textUpdate.enter().append("text").attr("x", xf).attr("y", yf).text(function (d) { return "(" + d.q.toString() + "," + d.r.toString() + ")"; });
            };
            Renderer.prototype.hoveredHex = function () {
                var location = d3.mouse(this.eventBox.node());
                var hex = this.hexFromXY(location[0], location[1]);
                return hex;
            };
            return Renderer;
        })();
        Frontend.Renderer = Renderer;
        function moveHexes(s, g) {
            return adjacentHexDirs(s).filter(function (hd) {
                var d = hd[1];
                var move = { segment: s, direction: d };
                return Abalone.Engine.validMove(g, move);
            }).map(function (hd) { return hd[0]; });
        }
        Frontend.moveHexes = moveHexes;
        function generateMove(s, target) {
            var possibilities = adjacentHexDirs(s);
            var foundDir;
            possibilities.forEach(function (hd) {
                var hex = hd[0];
                var dir = hd[1];
                if (JSON.stringify(hex) === JSON.stringify(target)) {
                    foundDir = dir;
                }
            });
            var move = foundDir != null ? { segment: s, direction: foundDir } : null;
            return move;
        }
        Frontend.generateMove = generateMove;
        function vanguard(pos, d) {
            return Abalone.Engine.nearbyDirections(d).map(function (dir) { return [Abalone.Engine.adjacent(pos, dir), dir]; });
        }
        function adjacentHexDirs(s) {
            if (s.orientation == null) {
                return Abalone.Engine.directions.map(function (d) { return [Abalone.Engine.adjacent(s.basePos, d), d]; });
            }
            else {
                var front = vanguard(s.basePos, Abalone.Engine.opposite(s.orientation));
                var pieces = Abalone.Engine.segPieces(s);
                var lastPiece = pieces[pieces.length - 1];
                var back = vanguard(lastPiece, s.orientation);
                return front.concat(back);
            }
        }
        function hexPointString(size, x, y) {
            var s = "";
            for (var i = 0; i < 6; i++) {
                s += hexCorner(x, y, size, i);
                s += " ";
            }
            return s;
        }
        function hexCorner(x, y, size, i) {
            var angle = 2 * Math.PI / 6 * (i + 0.5);
            return [x + size * Math.cos(angle), y + size * Math.sin(angle)];
        }
    })(Frontend = Abalone.Frontend || (Abalone.Frontend = {}));
})(Abalone || (Abalone = {}));
