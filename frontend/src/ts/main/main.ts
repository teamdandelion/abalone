window.onload = () => {
	var svg = d3.select("body").append("svg").attr("width", 800).attr("height", 800);
	var renderer = new Abalone.Renderer(svg, 800, 800);
	var xhr: any = d3.xhr("http://localhost:1337/frontend", "appliation/json");
	xhr.post("", (err, resp) => {
		var game = Abalone.deserializeGame(resp.response);
		var white: Abalone.PlayerAgent;
		var black: Abalone.PlayerAgent;
		if (game.nextPlayer === Abalone.Player.White) {
			white = renderer;
			black = new Abalone.RemotePlayer("1337");
		} else {
			white = new Abalone.RemotePlayer("1337");
			black = renderer;
		}
		var interactiveGame = new Abalone.InteractiveGame(renderer, white, black);
		interactiveGame.start(game);
	});
}
