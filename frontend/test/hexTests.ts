var assert = chai.assert;

module Abalone {

describe("Hex", () => {
	it("hexagonal grid seems to work", () => {
		assert.lengthOf(Hex.hexagonalGrid(1), 1);
		assert.lengthOf(Hex.hexagonalGrid(2), 7);
		assert.lengthOf(Hex.hexagonalGrid(5), 61);
	});
});
}
