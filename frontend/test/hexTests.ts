var assert = chai.assert;

module Abalone {

describe("Hex", () => {
	it("hexagonal grid seems to work", () => {
		assert.lengthOf(Hex.hexagonalGrid(1), 1);
		assert.lengthOf(Hex.hexagonalGrid(2), 7);
		assert.lengthOf(Hex.hexagonalGrid(5), 61);
	});

	it("onboard works", () => {
		var b1 = {whitePositions: [], blackPositions: [], boardRadius: 1};
		var b2 = {whitePositions: [], blackPositions: [], boardRadius: 2};
		var b3 = {whitePositions: [], blackPositions: [], boardRadius: 3};
		assert.isTrue (Hex.onBoard(b1, [0,0]), "b1, 00");
		assert.isFalse(Hex.onBoard(b1, [0,1]), "b1, 01");
		assert.isTrue (Hex.onBoard(b2, [0,1]), "b2, 01");
		assert.isFalse(Hex.onBoard(b2, [0,2]), "b2, 02");
		assert.isTrue (Hex.onBoard(b3, [0,2]), "b3, 02");
		assert.isFalse(Hex.onBoard(b3, [0,3]), "b3, 03");

		var b5 = {whitePositions: [], blackPositions: [], boardRadius: 5};
		Hex.hexagonalGrid(5).forEach((h) => assert.isTrue(Hex.onBoard(b5, h), h.toString()));
		Hex.ring(6).forEach((h) => assert.isFalse(Hex.onBoard(b5, h), h.toString()));
	});

	it("dist works (for at least colinear points)", () => {
		var h1 = [0,0];
		var h2 = [1,0];
		var h3 = [-2, 0]
		var h4 = [2,-2];

		assert.equal(Hex.dist(h1, h1), 0);
		assert.equal(Hex.dist(h1, h2), 1);
		assert.equal(Hex.dist(h2, h1), 1);
		assert.equal(Hex.dist(h1, h3), 2);
		assert.equal(Hex.dist(h2, h3), 3);
		assert.equal(Hex.dist(h1, h4), 2);

	});
});
}
