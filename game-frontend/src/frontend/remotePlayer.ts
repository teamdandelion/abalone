module Abalone {
export module Frontend {
	export class RemotePlayer implements PlayerAgent {
		constructor(private port: string) {}
		public play(g: Engine.Game, cb: (g: Engine.Game) => void): void {
			console.log("RemotePlayer - Play:", g);
			var gameToSend = Engine.serializeGame(g);
			var xhr : any = d3.xhr("http://localhost:" + this.port + "/frontend", "application/json");
			xhr.post(gameToSend, (err, resp) => {
				var xhr : any = d3.xhr("http://localhost:" + this.port + "/frontend", "application/json");
					if (err) console.log("remote player - outer:", err);
				xhr.get((err, resp) => {
					console.log("RemotePlayer - Rec game back:", resp.response);
					if (err) console.log("remote player - inner:", err);
					var responseGame = Engine.deserializeGame(resp.response);
					cb(responseGame)
					})
				});
		}
	}
}
}