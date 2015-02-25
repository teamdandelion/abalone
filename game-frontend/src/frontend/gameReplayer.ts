module Abalone {
export module Frontend {
	export class GameReplayer {
		public delay = 1000; // ms per step
		private controlLayer: D3.Selection;
		private playing = false;
		private idx = 0;
		private history: Engine.Game[];

		constructor(private renderer: Renderer, history: Engine.Game[]) {
			this.setHistory(history)
		}

		public setHistory(history: Engine.Game[]) {
			this.history = history;
			if (this.history.length > 0) {
				Engine.initializeIDs(this.history[0]);
				for (var i=1; i<this.history.length; i++) {
				    Engine.deduceIDs(this.history[i], this.history[i-1]);
				}
			}
		}

		public draw() {
			var state = this.history[this.idx]
			this.renderer.drawGame(state)
		}

		public back() {
			this.playing = false;
			if (this.idx === 0) return;
			this.idx--;
			this.draw();
		}

		public forward() {
			this.playing = false;
			if (this.idx === this.history.length - 1) return;
			this.idx++;
			this.draw();
		}

		public pause() {
			this.playing = false;
		}

		public play() {
			if (!this.playing) {
				this.playing = true;
				this.replay();
			}
		}

		public restart() {
			this.playing = false;
			this.idx = 0;
			this.draw();
		}

		public skipToEnd() {
			this.playing = false;
			this.idx = this.history.length - 1;
			this.draw();
		}

		public speedUp() {
			this.delay /= 2;
		}

		public slowDown() {
			this.delay *= 2;
		}

		public replay() {
			if (this.idx === this.history.length -1) {
				this.playing = false;
			}
			if (this.playing) {
				this.draw()
				this.idx++;
				setTimeout(this.replay.bind(this), this.delay)
			}
		}

	}
}
}