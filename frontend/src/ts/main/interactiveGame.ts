module Main {
    var port = "1337"
    export class InteractiveGame {
        private lastState: Abalone.Game;
        constructor(private renderer: Renderer, 
                    private white: PlayerAgent, 
                    private black: PlayerAgent) {}

        public start(game: Abalone.Game) {
            this.gameLoop(game);
        }
        
        public gameLoop(game: Abalone.Game) {
            if (this.lastState != null && !Abalone.validFuture(this.lastState, game)) {
                debugger;
            }
            this.lastState = game;
            this.renderer.drawGame(game);
            
            if (Abalone.gameOver(game)) {
                return;
            }
            var agent = game.nextPlayer === Abalone.Player.White ? this.white : this.black;
            agent.play(game, this.gameLoop.bind(this));
        }
    }

    export function playLocalGame(svg: D3.Selection, state?: Abalone.Game): void {
        if (state == null) {
            state = Abalone.standardGame();
        }
        var renderer = new Renderer(svg, state.board.edgeLength);
        var interactiveGame = new InteractiveGame(renderer, renderer, renderer);
        interactiveGame.start(state);
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
