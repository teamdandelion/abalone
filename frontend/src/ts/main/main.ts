module Main {

	var port = "1337";

	function gameLocal(svg: D3.Selection, renderer: Renderer): void {
		var game = Abalone.standardGame();
		var interactiveGame = new InteractiveGame(renderer, renderer, renderer);
		interactiveGame.start(game);
	}

	function gameRemote(svg: D3.Selection, renderer: Renderer): void {
		var xhr: any = d3.xhr("http://localhost:" + port + "/frontend", "appliation/json");
		xhr.post("", (err, resp) => {
			var game = Abalone.deserializeGame(resp.response);
			var white: PlayerAgent;
			var black: PlayerAgent;
			if (game.nextPlayer === Abalone.Player.White) {
				white = renderer;
				black = new RemotePlayer(port);
			} else {
				white = new RemotePlayer(port);
				black = renderer;
			}
			var interactiveGame = new InteractiveGame(renderer, white, black);
			interactiveGame.start(game);
		});
		
	}

	window.onload = () => {
		var svg = d3.select("body").append("svg").attr("width", 800).attr("height", 800);
		var renderer = new Renderer(svg, 800, 800);
		gameLocal(svg, renderer);
	}
}