var assert = chai.assert;

module Abalone {

describe("Hex", () => {
	it("hexagonal grid seems to work", () => {
		assert.lengthOf(hexagonalGrid(1), 1);
		assert.lengthOf(hexagonalGrid(2), 7);
		assert.lengthOf(hexagonalGrid(5), 61);
	});

	it("onboard works", () => {
		var b1 = {whitePositions: [], blackPositions: [], boardRadius: 1};
		var b2 = {whitePositions: [], blackPositions: [], boardRadius: 2};
		var b3 = {whitePositions: [], blackPositions: [], boardRadius: 3};
		assert.isTrue (onBoard(b1, {q: 0, r: 0}), "b1, 00");
		assert.isFalse(onBoard(b1, {q: 0, r: 1}), "b1, 01");
		assert.isTrue (onBoard(b2, {q: 0, r: 1}), "b2, 01");
		assert.isFalse(onBoard(b2, {q: 0, r: 2}), "b2, 02");
		assert.isTrue (onBoard(b3, {q: 0, r: 2}), "b3, 02");
		assert.isFalse(onBoard(b3, {q: 0, r: 3}), "b3, 03");

		var b5 = {whitePositions: [], blackPositions: [], boardRadius: 5};
		hexagonalGrid(5).forEach((h) => assert.isTrue(onBoard(b5, h), JSON.stringify(h)));
		ring(6).forEach((h) => assert.isFalse(onBoard(b5, h), JSON.stringify(h)));
	});

	it("dist works (for at least colinear points)", () => {
		var h1 = {q: 0,  r: 0};
		var h2 = {q: 1,  r: 0};
		var h3 = {q: -2, r: 0};
		var h4 = {q: 2,  r: -2};

		assert.equal(dist(h1, h1), 0);
		assert.equal(dist(h1, h2), 1);
		assert.equal(dist(h2, h1), 1);
		assert.equal(dist(h1, h3), 2);
		assert.equal(dist(h2, h3), 3);
		assert.equal(dist(h1, h4), 2);

	});
});
}
