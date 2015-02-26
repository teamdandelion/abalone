/// <reference path="hex.ts" />
/// <reference path="player.ts" />
/// <reference path="outcome.ts" />
/// <reference path="segment.ts" />
/// <reference path="move.ts" />
/// <reference path="board.ts" />

module Abalone {
export module Engine {
	export interface Game {
		board: Board;
		nextPlayer: Player;
		movesRemaining: number;
		marblesPerMove: number;
		lossThreshold: number;
	}

	export function initializeIDs(game: Game) {
		var i = 0;
		game.board.whitePositions.forEach((h: Hex) => {
			h.id = i++;
		});
		game.board.blackPositions.forEach((h: Hex) => {
			h.id = i++;
		});
	}

	// Given a new game state without any IDs, and a previous game state with IDs,
	// deduce the IDs for the new game by assuming that a valid move transformed the 
	// previous state into the current state. This enables the renderer to animate
	// the transitions
	export function deduceIDs(currentState: Game, lastState: Game) {
		var idMap: any = {};
		var getID = (h: Hex) => idMap[hexstr(h)] = h.id;
		lastState.board.whitePositions.forEach(getID);
		lastState.board.blackPositions.forEach(getID);
		var generatingMove = findGeneratingMove(lastState, currentState);

		function getPieceMovements(g: Game, m: Move) {
			var ownPieces = segPieces(m.segment);
			var enemyPieces = broadside(m) ? [] : inlineMoved(g.board, m);
			return ownPieces.concat(enemyPieces)
		}
		var hexesToMove = getPieceMovements(lastState, generatingMove);
		var staging: any = {};
		hexesToMove.forEach((h) => {
			var k = hexstr(h);
			staging[k] = idMap[k];
			delete idMap[k];
		});
		hexesToMove.forEach((h) => {
			var originalK = hexstr(h);
			var movedK = hexstr(adjacent(h, generatingMove.direction));
			idMap[movedK] = staging[originalK];
		});

		var addID = (h: Hex) => h.id = idMap[hexstr(h)];
		currentState.board.whitePositions.forEach(addID);
		currentState.board.blackPositions.forEach(addID);
	}

	function removeIDs(game: Game) {
		game.board.whitePositions.forEach((h: Hex) => {
			delete h.id
		});
		game.board.blackPositions.forEach((h: Hex) => {
			delete h.id
		});
	}

	export function standardGame(): Game {
		return {
			lossThreshold : 8,
			marblesPerMove: 3,
			movesRemaining: 200,
			nextPlayer    : Player.White,
			board         : standardBoard()
		}
	}	

	export function smallGame(): Game {
		return {
			lossThreshold : 1,
			marblesPerMove: 3,
			movesRemaining: 200,
			nextPlayer    : Player.White,
			board         : smallBoard()
		}
	}	

	export function findGeneratingMove(initial: Game, future: Game): Move {
		var ms = moves(initial);
		for (var i=0; i<ms.length; i++) {
			var f = update(initial, ms[i]);
			if (gameEq(f, future)) {
				return ms[i]
			}
		}	
		return null;	
	}

	export function validFuture(initial: Game, future: Game): boolean {
		var theFutures = futures(initial);
		for (var i = 0; i < theFutures.length; i++) {
			if (gameEq(future, theFutures[i])) {
				return true;
			}
		}
		return false;
	}

	export function gameEq(g1: Game, g2: Game) {
		function boardEq(b1: Board, b2: Board) {
			return b1.edgeLength === b2.edgeLength 
				&& arrayIsPermutation(b1.whitePositions, b2.whitePositions)
				&& arrayIsPermutation(b1.blackPositions, b2.blackPositions);
		}
		function arrayIsPermutation(a1: Hex[], a2: Hex[]) {
			if (a1.length !== a2.length) return false;
            var present: any = {}
            var i: number;
            var k: string;
            for (i = 0; i < a1.length; i++) {
                k = hexstr(a1[i])
                present[k] = true;
            }
            for (i = 0; i < a2.length; i++) {
                k = hexstr(a2[i])
                if (!present[k]) {
                    return false;
                }
            }
            return true;
		}
		return g1.nextPlayer === g2.nextPlayer 
			&& g1.movesRemaining === g2.movesRemaining
			&& g1.lossThreshold === g2.lossThreshold
			&& g1.marblesPerMove === g2.marblesPerMove
			&& boardEq(g1.board, g2.board);
	}

	function copyJSON(a: any): any {
		return JSON.parse(JSON.stringify(a));
	}

	export function serializeGame(g: Game): string {
		var copiedState = copyJSON(g);
		removeIDs(copiedState);
		copiedState.nextPlayer = player2str(g.nextPlayer)
		return JSON.stringify(copiedState);
	}

	export function parseJSON(s: any): Game {
		s.nextPlayer = str2player(s.nextPlayer)
		return s;
	}

	export function deserializeGame(s: string): Game {
		var g = JSON.parse(s);
		if (g.state != null) {
			// HACKHACK brian changed format so a moverequest contains a game state
			g = g.state;
		}
		g.nextPlayer = str2player(g.nextPlayer);
		return <Game> g;
	}

	export function winner(g: Game): Outcome {
		if (gameOver(g)) {
			var w = g.board.whitePositions.length;
			var b = g.board.blackPositions.length;
			if (w > b) return Outcome.WhiteWins;
			if (b > w) return Outcome.BlackWins;
			return Outcome.TieGame;
		}
		return null;
	}

	export function moves(g: Game): Move[] {
		var allMoves = [];
		segments(g).forEach((s) => {
			directions.forEach((d) => {
				allMoves.push({segment: s, direction: d});
			});
		});
		return allMoves.filter((m) => validMove(g, m));
	}

	export function gameOver(g: Game): boolean {
		var b = g.board;
		return  g.movesRemaining <= 0 || Math.min(b.whitePositions.length, b.blackPositions.length)  <= g.lossThreshold;
	}

	export function segments(g: Game): Segment[] {
		var pieces = getPieces(g.board, g.nextPlayer);
		var presentSet: any = {};
		var singletons = pieces.map((pos) => {
			presentSet[hexstr(pos)] = true;
			return {basePos: pos, orientation: null, segLength: 1, player: g.nextPlayer}
		});

		var twoOrMore = [];
		pieces.forEach((pos) => {
			[Direction.TopRight, Direction.MidRight, Direction.BotRight].forEach((d) => {
				var nextPiece = adjacent(pos, d);
				var length = 2;
				while (length <= g.marblesPerMove && presentSet[hexstr(nextPiece)]) {
					twoOrMore.push({
						basePos: pos, 
						orientation: d, 
						segLength: length, 
						player: g.nextPlayer
					});
					nextPiece = adjacent(nextPiece, d);
					length++;
				}
			});
		});
		return singletons.concat(twoOrMore);
	}

	export function getSegment(g: Game, origin: Hex, destination?: Hex): Segment {
		function getProposedSegment(origin: Hex, destination?: Hex) {
			if (destination == null || hexstr(origin) === hexstr(destination)) {
				return {basePos: origin, segLength: 1, player: g.nextPlayer, orientation: null};
			}
			var d = findDirection(origin, destination);
			if (d != null) {
				return {
					basePos: origin, 
					segLength: dist(origin, destination) + 1,
					orientation: d,
					player: g.nextPlayer
				}
			}
		}

		var proposedSegment = getProposedSegment(origin, destination);

		var pieces = getPieces(g.board, g.nextPlayer);
		var pieceSet: any = {};
		pieces.forEach((p) => pieceSet[hexstr(p)] = true);
		var pieceChecker = (p: Hex) => {
			return pieceSet[hexstr(p)];
		}

		if (proposedSegment && segPieces(proposedSegment).every(pieceChecker)) {
			return proposedSegment;
		} else {
			return null;
		}
	}

	export function update(g: Game, m: Move): Game {
		var ownPieces = segPieces(m.segment);
		var enemyPieces = broadside(m) ? [] : inlineMoved(g.board, m);
		var whiteMoved = (g.nextPlayer === Player.White) ? ownPieces : enemyPieces;
		var blackMoved = (g.nextPlayer === Player.White) ? enemyPieces : ownPieces;

		var movePieces = (ps: Hex[]) => {
			return ps
				.map((p) => adjacent(p, m.direction))
				.filter((p) => onBoard(g.board, p));
		}

		function removeAll(source: Hex[], remove: Hex[]): Hex[] {
			var out = source.slice();
			remove.forEach((x) => {
				var idx = hexIndex(out, x);
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
		}

		return {
			board: newBoard, 
			nextPlayer: next(g.nextPlayer), 
			movesRemaining: g.movesRemaining - 1,
			marblesPerMove: g.marblesPerMove,
			lossThreshold: g.lossThreshold
		}
	}

	export function futures(g: Game): Game[] {
		return moves(g).map((m) => update(g, m));
	}
}
}

