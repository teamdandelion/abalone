/// <reference path="../build/abalone.d.ts" />
var assert = chai.assert;

module Abalone {
function board(white: number[][], black: number[][], rad: number): Board {
	return {
		whitePositions: tuplesToHexes(white), 
		blackPositions: tuplesToHexes(black), 
		boardRadius: rad
	};
}

function game(b: Board, np: Player, mr: number, lt: number, mpm: number): Game {
	return {
		board: b,
		nextPlayer: np,
		movesRemaining: mr,
		lossThreshold: lt,
		marblesPerMove: mpm
	}
}

function continueGame(g: Game, white: number[][], black: number[][]): Game {
	return {
		board: board(white, black, g.board.boardRadius),
		nextPlayer: next(g.nextPlayer),
		movesRemaining: g.movesRemaining-1,
		lossThreshold: g.lossThreshold,
		marblesPerMove: g.marblesPerMove
	}
}

function stdGame(b: Board): Game {
	return {
		board: b,
		nextPlayer: Player.White,
		movesRemaining: 1000,
		lossThreshold: 2,
		marblesPerMove: 3
	}
}

function stdGameB(white: number[][], black: number[][], rad: number): Game {
	return stdGame(board(white, black, rad));
}

function move(base: Hex, orientation: Direction, len: number, player: Player, dir: Direction): Move {
	return {
		segment: {
			basePos: base,
			orientation: orientation,
			segLength: len,
			player: player
		},
		direction: dir
	}
}

function gameEq(g1: Game, g2: Game) {
	function boardEq(b1: Board, b2: Board) {
		return b1.boardRadius === b2.boardRadius 
			&& tupleArraySetEq(b1.whitePositions, b2.whitePositions)
			&& tupleArraySetEq(b1.blackPositions, b2.blackPositions);
	}
	function tupleArraySetEq(a1: Hex[], a2: Hex[]) {
		if (a1.length !== a2.length) return false;
		var sort1 = a1.slice().sort();
		var sort2 = a2.slice().sort();
		for (var i=0; i<a1.length; i++) {
			if (!tupleEq(sort1[i], sort2[i])) return false;
		}
		return true;
	}
	function tupleEq(t1: Hex, t2: Hex) {
		return t1[0] === t2[0] && t1[1] === t2[1];
	}
	return g1.nextPlayer === g2.nextPlayer 
		&& g1.movesRemaining === g2.movesRemaining
		&& g1.lossThreshold === g2.lossThreshold
		&& g1.marblesPerMove === g2.marblesPerMove
		&& boardEq(g1.board, g2.board);
}

function assertGameEq(g1: Game, g2: Game, message: string) {
	assert.isTrue(gameEq(g1, g2), message);
}

describe("Abalone", () => {
	var trivialGame = stdGameB([[0,0]], [], 5);
	var twoStoneGame = stdGameB([[-1, 0], [0, 0]], [], 1);
	var threeStoneGame = stdGameB([[-1, 0], [0, 0]], [[1, 0]], 2);
	var threeStoneGameAfterPush = continueGame(threeStoneGame, [[0,0],[1,0]], []);

	var fourStoneGame = stdGameB([[-2,0],[-1,0],[1,0]], [[0,0]], 5);
	var balancedGame = stdGameB([  [-2, 0], [-1,0] ],  [ [0,0], [1,0]  ], 5)

	it("segments works correctly", () => {
		assert.lengthOf(segments(trivialGame), 1, "1 stone game");
		assert.lengthOf(segments(twoStoneGame), 3, "2 stone game");
		assert.lengthOf(segments(threeStoneGame), 3, "2 stones with opposing pieces");
	});

	it("inlineMoved works as expected", () => {
		var whites2 = [[0,0], [0,1]];
		var blacks2 = [[0,-1], [0, -2]];
		var blacks1 = [[0,-1]];
		var cantpush = board(whites2, blacks2, 4);
		var canpush = board(whites2, blacks1, 4);
		var whites3 = [[0,0], [0,1], [0, -2]];
		var whiteblock = board(whites3, blacks1, 4);
		var m = move({q: 0,r: 0}, Direction.BotRight, 2, Player.White, Direction.TopLeft);
		assert.isNull(Abalone.inlineMoved(cantpush, m), "cant push equal # pieces");
		assert.deepEqual(Abalone.inlineMoved(canpush, m), [{q: 0, r:-1}], "can push 1 piece");
		assert.isNull(Abalone.inlineMoved(whiteblock, m), "push blocked by own piece");

	});

	it("right # of moves from ", () => {
		assert.lengthOf(moves(trivialGame), 6, "1 stone game");
		assert.lengthOf(moves(twoStoneGame), 16, "2 stone game");
		assert.lengthOf(moves(threeStoneGame), 15, "3 stone game");
		assert.lengthOf(moves(fourStoneGame), 19, "4 stone game (own piece protection)");
		assert.lengthOf(moves(balancedGame), 14, "balanced game (enemy piece protection)");
		
		var marblesPerMove = stdGameB([[0,1], [0,2], [0,3]], [], 5);
		assert.lengthOf(moves(marblesPerMove), 30, "marblesPerMove respected (3)");
		marblesPerMove.marblesPerMove = 1;
		assert.lengthOf(moves(marblesPerMove), 14, "marblesPerMove respected (1)");
	});

	describe("Abalone.update works", () => {
		it("basic moves update properly", () => {
			var before = stdGameB([[0,0]], [], 5);
			var m = move({q: 0, r: 0}, null, 0, Player.White, Direction.MidRight);
			var after = Abalone.update(before, m);
			var expected = continueGame(before, [[1,0]], []);
			assertGameEq(after, expected, "move updated");
		});

		it("slightly more complicated case updates", () => {
			var before = threeStoneGame;
			var m = move({q: -1, r: 0}, null, 0, Player.White, Direction.TopRight);
			var after = update(before, m);
			var expected = continueGame(before, [[0,-1], [0,0]], [[1,0]]);
			assertGameEq(after, expected, "move updated");
		});

		it("stones can be pushed off the board", () => {
			var moves = Abalone.moves(threeStoneGame);
			var futures = Abalone.futures(threeStoneGame);
			var found = futures.some((g) => gameEq(g, threeStoneGameAfterPush));
			assert.isTrue(found, "game with stone pushed off board is in futures");
		});
	});

	it("getSegment works", () => {
		var actual1 = getSegment(fourStoneGame, {q: -2, r: 0});
		var expected1 = {basePos: {q: -2, r: 0}, segLength: 1, orientation: null, player: Player.White};
		assert.deepEqual(actual1, expected1, "getSegment1");

		var actual2 = getSegment(fourStoneGame, {q: 0, r: 0});
		var expected2 = null;
		assert.deepEqual(actual2, expected2, "getSegment2");

		var actual3 = getSegment(fourStoneGame, {q: -2, r: 0}, {q: -1, r: 0});
		var expected3 = {basePos: {q: -2, r: 0}, segLength: 2, orientation: Direction.MidRight, player: Player.White};
		assert.deepEqual(actual3, expected3, "getSegment3");

		var actual4 = getSegment(fourStoneGame, {q: -1, r: 0}, {q: -2, r: 0});
		var expected4 = {basePos: {q: -1, r: 0}, segLength: 2, orientation: Direction.MidLeft, player: Player.White};
		assert.deepEqual(actual4, expected4, "getSegment4");

		var actual5 = getSegment(fourStoneGame, {q: -2, r: 0}, {q: 0, r: 0});
		var expected5 = null;
		assert.deepEqual(actual5, expected5, "getSegment5");

	});

	it("hexagonal grid seems to work", () => {
		assert.lengthOf(hexagonalGrid(1), 1);
		assert.lengthOf(hexagonalGrid(2), 7);
		assert.lengthOf(hexagonalGrid(5), 61);
	});
});
}
