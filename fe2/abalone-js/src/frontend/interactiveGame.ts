module Abalone {
export module Frontend {
    var port = "1337"
    export class InteractiveGame {
        private lastState: Engine.Game;
        constructor(private renderer: Renderer, 
                    private white: PlayerAgent, 
                    private black: PlayerAgent) {}

        public start(game: Engine.Game) {
            this.gameLoop(game);
        }
        
        public gameLoop(game: Engine.Game) {
            if (this.lastState != null && !Engine.validFuture(this.lastState, game)) {
                debugger;
            }
            if (this.lastState == null) {
                Engine.initializeIDs(game);
            } else {
                Engine.deduceIDs(game, this.lastState)
            }
            this.lastState = game;
            this.renderer.drawGame(game);
            
            if (Engine.gameOver(game)) {
                return;
            }
            var agent = game.nextPlayer === Engine.Player.White ? this.white : this.black;
            agent.play(game, this.gameLoop.bind(this));
        }
    }
}
}
