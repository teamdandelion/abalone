module Main {
	export class MarbleTracker {
		private identityMap: any;
		private state: Abalone.Game;

		constructor() {}

		private initialize(state: Abalone.Game) {
			this.identityMap = {}
			var i = 0;

			var addToMap = ((h: Abalone.Hex) => {
				this.identityMap[JSON.stringify(h)] = i++
			});

			state.board.whitePositions.forEach(addToMap);
			state.board.blackPositions.forEach(addToMap);
		}

		public update(future: Abalone.Game) {
			if (this.state == null) {
				this.initialize(future);
			} else {
				this._update(future);
			}
			this.state = future;
		}

		private _update(future: Abalone.Game) {
			var generatingMove = Abalone.findGeneratingMove(this.state, future);
			var hexesToMove = getPieceMovements(this.state, generatingMove);
			this.moveHexes(hexesToMove, generatingMove.direction);
		}

		public getID(h: Abalone.Hex): string {
			var id = this.identityMap[JSON.stringify(h)];
			return id.toString();
		}

		public getKeyedPieces(pieces: Abalone.Hex[]) {
			var keyedPieces = [];
			pieces.forEach((h) => {
				keyedPieces.push({
					q: h.q,
					r: h.r,
					id: this.identityMap[JSON.stringify(h)]
				});
			});
			return keyedPieces;
		}

		private moveHexes(hs: Abalone.Hex[], d: Abalone.Direction) {
			var staging: any = {}
			var keys = hs.map((h) => JSON.stringify(h))
			hs.forEach((h) => {
				var k = JSON.stringify(h);
				staging[k] = this.identityMap[k];
				delete this.identityMap[k];
			});
			hs.forEach((h) => {
				var originalK = JSON.stringify(h);
				var movedK = JSON.stringify(Abalone.adjacent(h, d));
				this.identityMap[movedK] = staging[originalK];
			});

		}

	}

	function getPieceMovements(g: Abalone.Game, m: Abalone.Move) {
		var ownPieces = Abalone.segPieces(m.segment);
		var enemyPieces = Abalone.broadside(m) ? [] : Abalone.inlineMoved(g.board, m);
		return ownPieces.concat(enemyPieces)
	}

}