window.onload = () => {
	var game = Abalone.standardGame();
	var svg = d3.select("body").append("svg").attr("width", 800).attr("height", 800);
	var renderer = new Abalone.Renderer(svg, 800, 800);
	renderer.drawGame(game);
}
