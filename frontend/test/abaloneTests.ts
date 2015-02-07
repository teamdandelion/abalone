var assert = chai.assert;

module Abalone {


function genTestBoard(white: any, black: any, rad: number): Board {
	return {
		whitePositions: white, 
		blackPositions: black, 
		boardRadius: rad
	};
}
function genTestGame(white: any, black: any, rad: number): Game {
	return {
		board: genTestBoard(white, black, rad),
		nextPlayer: Player.White,
		movesRemaining: 1000,
		lossThreshold: 2,
		marblesPerMove: 3
	}
}

function assertGameEq(g1: Game, g2: Game, message: string) {
	function gameEq(g1: Game, g2: Game) {
		return g1.nextPlayer === g2.nextPlayer 
			&& g1.movesRemaining === g2.movesRemaining
			&& g1.lossThreshold === g2.lossThreshold
			&& g1.marblesPerMove === g2.marblesPerMove
			&& boardEq(g1.board, g2.board);
	}
	function boardEq(b1: Board, b2: Board) {
		return b1.boardRadius === b2.boardRadius 
			&& tupleArraySetEq(b1.whitePositions, b2.whitePositions)
			&& tupleArraySetEq(b1.blackPositions, b2.blackPositions);
	}
	function tupleArraySetEq(a1: [number, number][], a2: [number, number][]) {
		if (a1.length !== a2.length) return false;
		var sort1 = a1.slice().sort();
		var sort2 = a2.slice().sort();
		for (var i=0; i<a1.length; i++) {
			if (!tupleEq(sort1[i], sort2[i])) return false;
		}
		return true;
	}
	function tupleEq(t1: [number, number], t2: [number, number]) {
		return t1[0] === t2[0] && t1[1] === t2[1];
	}
	assert.isTrue(gameEq(g1, g2), message);
}

describe("Abalone", () => {
	var trivialGame = genTestGame([[0,0]], [], 5);
	var twoStoneGame = genTestGame([[-1, 0], [0, 0]], [], 1);
	var threeStoneGame = genTestGame([[-1, 0], [0, 0]], [[1, 0]], 1);
	var threeStoneGameAfterPush = {
		board: genTestBoard([[0,0], [1,0]], [], 1),
		nextPlayer: Player.Black,
		movesRemaining: 999,
		marblesPerMove: 3,
		lossThreshold: 2
	};
	var fourStoneGame = genTestGame([[-2,0],[-1,0],[1,0]], [[0,0]], 5);
	var balancedGame = genTestGame([  [-2, 0], [-1,0] ],  [ [0,0], [1,0]  ], 5)

	it("segments works correctly", () => {
		assert.lengthOf(segments(trivialGame), 1, "1 stone game");
		assert.lengthOf(segments(twoStoneGame), 3, "2 stone game");
		assert.lengthOf(segments(threeStoneGame), 3, "2 stones with opposing pieces");
	});

	it("inlineMoved works as expected", () => {
		var whites2 = [[0,0], [0,1]];
		var blacks2 = [[0,-1], [0, -2]];
		var blacks1 = [[0,-1]];
		var cantpush = genTestBoard(whites2, blacks2, 4);
		var canpush = genTestBoard(whites2, blacks1, 4);
		var move = {
			segment: {basePos: <[number, number]> [0,0], segLength: 2, orientation: Direction.BotRight, player: Player.White},
			direction: Direction.TopLeft
		}
		assert.isNull(Abalone.inlineMoved(cantpush, move), "cant push equal # pieces");
		assert.deepEqual(Abalone.inlineMoved(canpush, move), [[0,-1]], "can push 1 piece");

	});

	it("right # of moves from ", () => {
		assert.lengthOf(possibleMoves(trivialGame), 6, "1 stone game");
		assert.lengthOf(possibleMoves(twoStoneGame), 16, "2 stone game");
		assert.lengthOf(possibleMoves(threeStoneGame), 15, "3 stone game");
		assert.lengthOf(possibleMoves(fourStoneGame), 19, "4 stone game (own piece protection)");
		assert.lengthOf(possibleMoves(balancedGame), 19, "balanced game (enemy piece protection)");
		
		var marblesPerMove = genTestGame([[0,1], [0,2], [0,3]], [], 5);
		assert.lengthOf(possibleMoves(marblesPerMove), 30, "marblesPerMove respected (3)");
		marblesPerMove.marblesPerMove = 1;
		assert.lengthOf(possibleMoves(marblesPerMove), 14, "marblesPerMove respected (1)");
	});

	//it("stones can be pushed off the board", () => {})

	it("hexagonal grid seems to work", () => {
		assert.lengthOf(Hex.hexagonalGrid(1), 1);
		assert.lengthOf(Hex.hexagonalGrid(2), 7);
		assert.lengthOf(Hex.hexagonalGrid(5), 61);
	});
});
}
