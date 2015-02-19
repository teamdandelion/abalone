module Main {
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
}
