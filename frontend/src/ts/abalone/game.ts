/// <reference path="hex.ts" />
/// <reference path="player.ts" />
/// <reference path="outcome.ts" />
/// <reference path="segment.ts" />
/// <reference path="move.ts" />
/// <reference path="board.ts" />

module Abalone {
	export interface Game {
		board: Board;
		nextPlayer: Player;
		movesRemaining: number;
		marblesPerMove: number;
		lossThreshold: number;
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

	function copyJSON(a: any): any {
		return JSON.parse(JSON.stringify(a));
	}

	export function serializeGame(g: Game): string {
		var anyg = copyJSON(g);
		anyg.nextPlayer = player2str(g.nextPlayer)
		return JSON.stringify(anyg);
	}

	export function deserializeGame(s: string): Game {
		var g = JSON.parse(s);
		if (g.state != null) {
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
			presentSet[JSON.stringify(pos)] = true;
			return {basePos: pos, orientation: null, segLength: 1, player: g.nextPlayer}
		});

		var twoOrMore = [];
		pieces.forEach((pos) => {
			[Direction.TopRight, Direction.MidRight, Direction.BotRight].forEach((d) => {
				var nextPiece = adjacent(pos, d);
				var length = 2;
				while (length <= g.marblesPerMove && presentSet[JSON.stringify(nextPiece)]) {
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
			if (destination == null || JSON.stringify(origin) === JSON.stringify(destination)) {
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
		pieces.forEach((p) => pieceSet[JSON.stringify(p)] = true);
		var pieceChecker = (p: Hex) => {
			return pieceSet[JSON.stringify(p)];
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

