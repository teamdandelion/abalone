declare module Abalone {
    module Engine {
        interface Hex {
            q: number;
            r: number;
            id?: number;
        }
        function hexIndex(hexes: Hex[], x: Hex): number;
        function hexagonalGrid(hexesOnEdge: number): Hex[];
        function hexstr(h: Hex): string;
        function ring(rad: number): Hex[];
        function nearbyDirections(d: Direction): Direction[];
        function findDirection(p1: Hex, p2: Hex): Direction;
        function dist2(x1: Hex, x2: Hex): number;
        function dist(x1: Hex, x2: Hex): number;
        function onBoard(b: Board, p: Hex): boolean;
        function opposite(d: Direction): Direction;
        function colinear(d1: Direction, d2: Direction): boolean;
        function adjacent(position: Hex, d: Direction): Hex;
    }
}
declare module Abalone {
    module Engine {
        enum Player {
            White = 0,
            Black = 1,
        }
        function next(p: Player): Player;
        function player2str(p: Player): string;
        function str2player(s: String): Player;
    }
}
declare module Abalone {
    module Engine {
        interface Board {
            whitePositions: Hex[];
            blackPositions: Hex[];
            edgeLength: number;
        }
        function free(b: Board, x: Hex): boolean;
        function owner(b: Board, x: Hex): Player;
        function getPieces(b: Board, p: Player): Hex[];
        function tuplesToHexes(tups: number[][]): Hex[];
        function standardBoard(): Board;
        function smallBoard(): Board;
    }
}
declare module Abalone {
    module Engine {
        enum Direction {
            TopRight = 0,
            MidRight = 1,
            BotRight = 2,
            TopLeft = 3,
            MidLeft = 4,
            BotLeft = 5,
        }
        var directions: Direction[];
    }
}
declare module Abalone {
    module Engine {
        enum Outcome {
            WhiteWins = 0,
            BlackWins = 1,
            TieGame = 2,
        }
    }
}
declare module Abalone {
    module Engine {
        interface Segment {
            basePos: Hex;
            orientation: Direction;
            segLength: number;
            player: Player;
        }
        function segPieces(s: Segment): Hex[];
    }
}
declare module Abalone {
    module Engine {
        interface Move {
            segment: Segment;
            direction: Direction;
        }
        function validMove(g: Game, m: Move): boolean;
        function broadside(m: Move): boolean;
        function inlineMoved(b: Board, m: Move): Hex[];
    }
}
declare module Abalone {
    module Engine {
        interface Game {
            board: Board;
            nextPlayer: Player;
            movesRemaining: number;
            marblesPerMove: number;
            lossThreshold: number;
        }
        function initializeIDs(game: Game): void;
        function deduceIDs(currentState: Game, lastState: Game): void;
        function standardGame(): Game;
        function smallGame(): Game;
        function findGeneratingMove(initial: Game, future: Game): Move;
        function validFuture(initial: Game, future: Game): boolean;
        function gameEq(g1: Game, g2: Game): boolean;
        function serializeGame(g: Game): string;
        function parseJSON(s: any): Game;
        function deserializeGame(s: string): Game;
        function winner(g: Game): Outcome;
        function moves(g: Game): Move[];
        function gameOver(g: Game): boolean;
        function segments(g: Game): Segment[];
        function getSegment(g: Game, origin: Hex, destination?: Hex): Segment;
        function update(g: Game, m: Move): Game;
        function futures(g: Game): Game[];
    }
}
declare module Abalone {
    module Frontend {
        class GameReplayer {
            delay: number;
            private controlLayer;
            private playing;
            private idx;
            private history;
            private renderer;
            setRenderer(r: Renderer): void;
            setHistory(history: Engine.Game[]): void;
            draw(): void;
            back(): void;
            forward(): void;
            pause(): void;
            play(): void;
            restart(): void;
            skipToEnd(): void;
            speedUp(): void;
            slowDown(): void;
            replay(): void;
        }
    }
}
declare module Abalone {
    module Frontend {
        class InteractiveGame {
            private renderer;
            private white;
            private black;
            private lastState;
            constructor(renderer: Renderer, white: PlayerAgent, black: PlayerAgent);
            start(game: Engine.Game): void;
            gameLoop(game: Engine.Game): void;
        }
    }
}
declare module Abalone {
    module Frontend {
        class LocalPlayer implements PlayerAgent {
            private renderer;
            constructor(renderer: Renderer);
            play(g: Engine.Game, cb: (g: Engine.Game) => void): void;
        }
    }
}
declare module Abalone {
    module Frontend {
        interface PlayerAgent {
            play(g: Engine.Game, cb: (g: Engine.Game) => void): void;
        }
    }
}
declare module Abalone {
    module Frontend {
        class RemotePlayer implements PlayerAgent {
            private port;
            constructor(port: string);
            play(g: Engine.Game, cb: (g: Engine.Game) => void): void;
        }
    }
}
declare module Abalone {
    module Frontend {
        class Renderer {
            private svg;
            private board;
            private whitePieces;
            private blackPieces;
            private whiteNumPieces;
            private blackNumPieces;
            eventBox: D3.Selection;
            private coordinateLayer;
            private grid;
            private hexesOnEdge;
            private height;
            private width;
            private hexSize;
            constructor(svg: any, hexesOnEdge?: number);
            private autoGetWidthHeight();
            resize(width: any, height: any): void;
            private qr2xy(h);
            drawGame(g: Engine.Game): void;
            private drawPieces(b);
            private addPieces(selection, pieces);
            private hexFromXY(x, y);
            private hexRound(q, r);
            drawOverlay(segment: Engine.Segment, isDragging: boolean, game: Engine.Game): void;
            private highlightHexes(hexes, classToApply);
            private drawBoard();
            hoveredHex(): Engine.Hex;
        }
        function moveHexes(s: Engine.Segment, g: Engine.Game): Engine.Hex[];
        function generateMove(s: Engine.Segment, target: Engine.Hex): Engine.Move;
    }
}
