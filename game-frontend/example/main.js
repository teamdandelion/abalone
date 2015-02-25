function playLocalGame(svg, state) {
    if (state == null) {
        state = Abalone.Engine.standardGame();
    }
    var renderer = new Abalone.Frontend.Renderer(svg, state.board.edgeLength);
    var white = new Abalone.Frontend.LocalPlayer(renderer)
    var black = new Abalone.Frontend.LocalPlayer(renderer)
    var interactiveGame = new Abalone.Frontend.InteractiveGame(renderer, white, black);
    interactiveGame.start(state);
}

function replayGame(svg, games) {
    var renderer = new Abalone.Frontend.Renderer(svg);
    var replayer = new Abalone.Frontend.GameReplayer(renderer, games);
    replayer.play();
}

var port = 1337;
function playRemoteGame(svg) {
    var renderer = new Abalone.Frontend.Renderer(svg);
    var xhr = d3.xhr("http://localhost:" + port + "/frontend", "appliation/json");
    xhr.get(function(err, resp) {
        var game = Abalone.Engine.deserializeGame(resp.response);
        var white;
        var black;
        if (game.nextPlayer === Abalone.Engine.Player.White) {
            white = new Abalone.Frontend.LocalPlayer(renderer);
            black = new Abalone.Frontend.RemotePlayer(port);
        } else {
            white = new Abalone.Frontend.RemotePlayer(port);
            black = new Abalone.Frontend.LocalPlayer(renderer);
        }
        var interactiveGame = new Abalone.Frontend.InteractiveGame(renderer, white, black);
        interactiveGame.start(game);
    });
}