module Abalone {
    export class InteractiveGame {
        constructor(private renderer: Renderer, 
                    private white: PlayerAgent, 
                    private black: PlayerAgent) {}

        public start(game: Game) {
            this.gameLoop(game);
        }
        
        public gameLoop(game: Game) {
            this.renderer.drawGame(game);
            
            if (gameOver(game)) {
                return;
            }
            var agent = game.nextPlayer === Player.White ? this.white : this.black;
            agent.play(game, this.gameLoop.bind(this));
        }
    }
}
