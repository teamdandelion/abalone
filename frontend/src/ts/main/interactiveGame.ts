(<any> window).games = []
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
            (<any> window).games.push(game);
            if (this.lastState != null && !Abalone.validFuture(this.lastState, game)) {
                debugger;
            }
            if (this.lastState == null) {
                Abalone.initializeIDs(game);
            } else {
                Abalone.deduceIDs(game, this.lastState)
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
        var white = new LocalPlayer(renderer)
        var black = new LocalPlayer(renderer)
        var interactiveGame = new InteractiveGame(renderer, white, black);
        interactiveGame.start(state);
    }

    export function replayGame(svg: D3.Selection, games: Abalone.Game[]) {
        var renderer = new Renderer(svg);
        var replayer = new GameReplayer(renderer, games);
        (<any>window).r = replayer;
        replayer.play();

    }

    export function playRemoteGame(svg: D3.Selection): void {
        var renderer = new Renderer(svg);
        var xhr: any = d3.xhr("http://localhost:" + port + "/frontend", "appliation/json");
        xhr.get((err, resp) => {
            var game = Abalone.deserializeGame(resp.response);
            var white: PlayerAgent;
            var black: PlayerAgent;
            if (game.nextPlayer === Abalone.Player.White) {
                white = new LocalPlayer(renderer);
                black = new RemotePlayer(port);
            } else {
                white = new RemotePlayer(port);
                black = new LocalPlayer(renderer);
            }
            var interactiveGame = new InteractiveGame(renderer, white, black);
            interactiveGame.start(game);
        });

    }
}
