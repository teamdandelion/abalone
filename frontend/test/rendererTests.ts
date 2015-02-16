var assert = chai.assert;

module Abalone {

describe("Abalone.Renderer", () => {
	it("generateMove works", () => {
		var s1 = {basePos: [0,0], segLength: 1, orientation: null, player: Player.White};
		assert.isNull(generateMove(s1, [0,0]), "s1 : 0,0");
		assert.deepEqual(generateMove(s1, [-1,0]), {segment: s1, direction: Direction.MidLeft}, "s1 : 0,-1");
		assert.deepEqual(generateMove(s1, [1,-1]), {segment: s1, direction: Direction.TopRight}, "s1 : 1,-1");

		var s2 = {basePos: [-1,0], segLength: 3, orientation: Direction.MidRight, player: Player.White};
		assert.isNull(generateMove(s2, [0,0]), "s2:0,0");
		assert.isNull(generateMove(s2, [-1,1]), "s2:-1,1");
		assert.isNotNull(generateMove(s2, [-2,0]), "s2:-2,0");
		assert.isNotNull(generateMove(s2, [1,1]), "s2:1,1")

	});
});
}
