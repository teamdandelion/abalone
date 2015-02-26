var assert = chai.assert;

module Abalone {
export module Frontend {

describe("Main.Renderer", () => {
	it("generateMove works", () => {
		var s1 = {basePos: {q: 0, r: 0}, segLength: 1, orientation: null, player: Abalone.Engine.Player.White};
		assert.isNull(generateMove(s1, {q: 0, r: 0}), "s1 : 0,0");
		assert.deepEqual(generateMove(s1, {q: -1, r: 0}), {segment: s1, direction: Abalone.Engine.Direction.MidLeft}, "s1 : 0,-1");
		assert.deepEqual(generateMove(s1, {q: 1, r: -1}), {segment: s1, direction: Abalone.Engine.Direction.TopRight}, "s1 : 1,-1");

		var s2 = {basePos: {q: -1, r: 0}, segLength: 3, orientation: Abalone.Engine.Direction.MidRight, player: Abalone.Engine.Player.White};
		assert.isNull(generateMove(s2, {q: 0, r: 0}), "s2:0,0");
		assert.isNull(generateMove(s2, {q: -1, r: 1}), "s2:-1,1");
		assert.isNotNull(generateMove(s2, {q: -2, r: 0}), "s2:-2,0");
		assert.isNotNull(generateMove(s2, {q: 1, r: 1}), "s2:1,1")

	});
});
}
}
