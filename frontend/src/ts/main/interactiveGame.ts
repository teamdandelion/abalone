module Main {
    var port = "1337"
    export class InteractiveGame {
        constructor(private renderer: Renderer, 
                    private white: PlayerAgent, 
                    private black: PlayerAgent) {}

        public start(game: Abalone.Game) {
            this.gameLoop(game);
        }
        
        public gameLoop(game: Abalone.Game) {
            console.log("gameLoop:", game);
            this.renderer.drawGame(game);
            
            if (Abalone.gameOver(game)) {
                return;
            }
            var agent = game.nextPlayer === Abalone.Player.White ? this.white : this.black;
            agent.play(game, this.gameLoop.bind(this));
        }
    }

    export function playLocalGame(svg: D3.Selection): void {
        var renderer = new Renderer(svg);
        var game = Abalone.standardGame();
        var interactiveGame = new InteractiveGame(renderer, renderer, renderer);
        interactiveGame.start(game);
    }

    export function playRemoteGame(svg: D3.Selection): void {
        var renderer = new Renderer(svg);
        var xhr: any = d3.xhr("http://localhost:" + port + "/frontend", "appliation/json");
        xhr.get((err, resp) => {
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
}
