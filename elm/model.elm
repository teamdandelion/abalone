import Set

type alias Game = { board          : Board
                  , nextPlayer     : Player
                  , movesRemaining : Int
                  , marblesPerMove : Int
                  }

type alias Board = { whitePositions : Set.Set Position
                   , blackPositions : Set.Set Position
                   , boardRadius    : Int
                   }

type Direction = TopRight | MidRight | BotRight | TopLeft | MidLeft | BotLeft

type alias Move =  { segment   : Segment
                   , direction : Direction
                   } 

type alias Segment =  { basePos     : Position  
                      , orientation : Direction 
                      , segLength   : Int       
                      }                 

type alias Position = (Int, Int)

type Player = White | Black 

