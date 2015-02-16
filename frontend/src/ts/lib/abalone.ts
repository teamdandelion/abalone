
module Abalone {
	export enum Player {White, Black}
	export function next(p: Player) {
		return (p === Player.White) ? Player.Black : Player.White;
	}
	export enum Outcome {WhiteWins, BlackWins, TieGame}
	export enum Direction {TopRight, MidRight, BotRight, TopLeft, MidLeft, BotLeft}
	export interface Game {
		board: Board;
		nextPlayer: Player;
		movesRemaining: number;
		marblesPerMove: number;
		lossThreshold: number;
	}

	export interface Board {
		whitePositions: number[][];
		blackPositions: number[][];
		boardRadius: number;
	}

	export interface Segment {
		basePos: number[];
		orientation: Direction;
		segLength: number;
		player: Player;
	}

	export interface Move {
		segment: Segment;
		direction: Direction;
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

	export function getPieces(b: Board, p: Player): number[][] {
		if (p === Player.White) {
			return b.whitePositions;
		} else {
			return b.blackPositions;
		}
	}

	export function gameOver(g: Game): boolean {
		var b = g.board;
		return  g.movesRemaining <= 0 || Math.min(b.whitePositions.length, b.blackPositions.length)  <= g.lossThreshold;
	}

	export function possibleMoves(g: Game): Move[] {
		var allMoves = [];
		segments(g).forEach((s) => {
			Hex.directions.forEach((d) => {
				allMoves.push({segment: s, direction: d});
			});
		});
		return allMoves.filter((m) => isValid(g, m));
	}

	export function segments(g: Game): Segment[] {
		var pieces = getPieces(g.board, g.nextPlayer);
		var presentSet: any = {};
		var singletons = pieces.map((pos) => {
			presentSet[pos.toString()] = true;
			return {basePos: pos, orientation: null, segLength: 1, player: g.nextPlayer}
		});

		var twoOrMore = [];
		pieces.forEach((pos) => {
			[Direction.TopRight, Direction.MidRight, Direction.BotRight].forEach((d) => {
				var nextPiece = Hex.adjacent(pos, d);
				var length = 2;
				while (length <= g.marblesPerMove && presentSet[nextPiece.toString()]) {
					twoOrMore.push({
						basePos: pos, 
						orientation: d, 
						segLength: length, 
						player: g.nextPlayer
					});
					nextPiece = Hex.adjacent(nextPiece, d);
					length++;
				}
			});
		});
		return singletons.concat(twoOrMore);
	}

	export function getSegment(g: Game, origin: number[], destination?: number[]): Segment {
		function getProposedSegment(origin: number[], destination?: number[]) {
			if (destination == null || origin.toString() === destination.toString()) {
				return {basePos: origin, segLength: 1, player: g.nextPlayer, orientation: null};
			}
			var d = Hex.findDirection(origin, destination);
			if (d != null) {
				return {
					basePos: origin, 
					segLength: Hex.dist(origin, destination) + 1,
					orientation: d,
					player: g.nextPlayer
				}
			}
		}

		var proposedSegment = getProposedSegment(origin, destination);

		var pieces = getPieces(g.board, g.nextPlayer);
		var pieceSet: any = {};
		pieces.forEach((p) => pieceSet[p.toString()] = true);
		var pieceChecker = (p: number[]) => {
			return pieceSet[p.toString()];
		}

		if (proposedSegment && segPieces(proposedSegment).every(pieceChecker)) {
			return proposedSegment;
		} else {
			return null;
		}
	}

	export function isValid(g: Game, m: Move): boolean {
		if (broadside(m)) {
			return segPieces(m.segment)
				.map((p) => Hex.adjacent(p, m.direction))
				.every((p) => free(g.board, p));
		} else {
			return inlineMoved(g.board, m) !== null;
		}
	}

	export function segPieces(s: Segment): number[][] {
		if (s == null) return [];
		var front = s.basePos;
		var pieces = [front];
		for (var i=0; i<s.segLength-1; i++) {
			front = Hex.adjacent(front, s.orientation);
			pieces.push(front);
		}
		return pieces;
	}

	function free(b: Board, x: number[]): boolean {
		return owner(b,x) == null;
	}

	function tupleIndexOf(tuples: number[][], x: number[]): number {
		for (var i=0; i<tuples.length; i++) {
			if (tuples[i][0] === x[0] && tuples[i][1] === x[1]) return i;
		}
		return -1;
	}

	function owner(b: Board, x: number[]): Player {
		if (tupleIndexOf(b.whitePositions, x) !== -1) return Player.White;
		if (tupleIndexOf(b.blackPositions, x) !== -1) return Player.Black;
		return null;
	}

	export function update(g: Game, m: Move): Game {
		var ownPieces = segPieces(m.segment);
		var enemyPieces = broadside(m) ? [] : inlineMoved(g.board, m);
		var whiteMoved = (g.nextPlayer === Player.White) ? ownPieces : enemyPieces;
		var blackMoved = (g.nextPlayer === Player.White) ? enemyPieces : ownPieces;

		var movePieces = (ps: number[][]) => {
			return ps
				.map((p) => Hex.adjacent(p, m.direction))
				.filter((p) => Hex.onBoard(g.board, p));
		}

		function removeAll(source: number[][], remove: number[][]): number[][] {
			var out = source.slice();
			remove.forEach((x) => {
				var idx = tupleIndexOf(out, x);
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
			boardRadius: g.board.boardRadius
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
		return possibleMoves(g).map((m) => update(g, m));
	}

	function inline(m: Move): boolean {
		return m.segment.orientation !== null && Hex.colinear(m.direction, m.segment.orientation)
	}

	function broadside(m: Move): boolean {
		return !inline(m);
	}

	export function inlineMoved(b: Board, m: Move): number[][] {
		if (broadside(m)) return null;
		var pieces = segPieces(m.segment);
		var attacked = m.segment.orientation === m.direction ? pieces[pieces.length-1] : pieces[0];
		var movedEnemyPieces = [];
		for (var i=0; i<m.segment.segLength; i++) {
			attacked = Hex.adjacent(attacked, m.direction);
			var controller = owner(b, attacked);
			if (controller == null) return movedEnemyPieces;
			if (controller === m.segment.player) return null;
			movedEnemyPieces.push(attacked);
		}
		return null;
	}

		// 	getPieces(g.board, next(g.nextPlayer));
		// var ownSet: any = {};
		// ownPieces.forEach((p) => ownSet[p] = true);
		// var enemySet: any = {};
		// enemyPieces.forEach((p) => enemySet[p] = true);
		// var updated = 

	function standardBoard() {
		return <Board> <any> {
			boardRadius   : 5, 
			whitePositions:[[-4,3],[-4,4],[-3,3],[-3,4],[-2,2],[-2,3],[-2,4],[-1,2],[-1,3],[-1,4],[0,2],[0,3],[0,4],[1,3]], 
			blackPositions:[[-1,-3],[0,-4],[0,-3],[0,-2],[1,-4],[1,-3],[1,-2],[2,-4],[2,-3],[2,-2],[3,-4],[3,-3],[4,-4],[4,-3]]
		}
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
}
